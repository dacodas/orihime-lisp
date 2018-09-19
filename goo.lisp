(in-package :goo)

(log:config :debug)

(defvar *sentences* 
    (make-hash-table :test #'equalp))

(defvar *words* 
    (make-hash-table :test #'equal))

(defclass goo-sentence ()
  ((sentence
    :initarg :sentence
    :initform (error "Need to supply the sentence contents")
    :accessor goo-sentence-contents)
   (child-words
    :initform nil
    :accessor goo-sentence-children)
   (source
    :initform nil
    :accessor goo-sentence-source)))

(defun insert-sentence (sentence)

  (let* ((key (cl-base64:usb8-array-to-base64-string 
               (ironclad:digest-sequence :sha256 (sb-ext:string-to-octets (goo-sentence-contents sentence)))))
         (document (cl-mongo:make-document :oid key)))

    (cl-mongo:add-element "child-words" (mapcar #'goo-word-reading  (goo-sentence-children sentence)) document)
    (cl-mongo:add-element "sentence-contents" (goo-sentence-contents sentence) document)
    (cl-mongo:add-element "source" (goo-sentence-source sentence) document)
    (cl-mongo:db.save "sentences" document)))

(defclass goo-word ()
  ((reading
    :initarg :reading
    :accessor goo-word-reading)
   (results-page
    :initform nil)
   (entry-page
    :initform nil)
   (parent-words
    :initform nil
    :accessor goo-word-parents)
   (child-words
    :initform nil
    :accessor goo-word-children)))

;; We can check types using something like the following, taken from
;; https://stackoverflow.com/questions/12556513/clos-how-to-make-a-slot-have-an-enforced-type-of-vector-of-symbols

;; Further more, if we add type declarations to the slots we can use
;; sb-mop:slot-definition-type to get the type 

;; (defclass dacoda-test-class ()
;;   ((test-slot
;;     :initarg test-slot
;;     :initform nil
;;     :accessor test-slot)))

;; (defmethod (setf test-slot) :before (new-value (dacoda-test-object dacoda-test-class))
;;   (check-type new-value (unsigned-byte 8)))

(defun insert-word (word)

  (defun serialize (node)
    (let ((output (make-string-output-stream)))
      (plump:serialize node output)
      (get-output-stream-string output)))
  
  (let ((document (cl-mongo:make-document :oid (goo-word-reading word))))
    (cl-mongo:add-element "results-page" (serialize (slot-value word 'results-page)) document)
    (cl-mongo:add-element "entry-page" (serialize (slot-value word 'entry-page)) document)
    (cl-mongo:add-element "parent-words" (goo-word-parents word) document)
    (cl-mongo:add-element "child-words" (goo-word-children word) document)
    (cl-mongo:db.save "words" document)))

(defun read-word (word-reading)
  (cl-mongo:db.find "words" (cl-mongo:kv "_id" word-reading)))

(defun sentence-from-string (string)
  (gethash (ironclad:digest-sequence :sha256 (sb-ext:string-to-octets string)) *sentences*))

(defun add-sentence (sentence)
  (setf (gethash (ironclad:digest-sequence :sha256 (sb-ext:string-to-octets sentence)) *sentences*)
        ;; (sentence-from-string sentence) ;; <- Can't use this because of how setf works
        (make-instance 'goo-sentence :sentence sentence)))

(defun strip (string)
  (string-trim '(#\Space #\Tab #\Newline #\Linefeed #\Rubout #\Return) string))

(defun format-html-as-text (string-to-format)
  (let* ((input-stream (make-string-input-stream string-to-format))
         (process (sb-ext:run-program "/usr/bin/html2text" '("--ignore-links")
                                      :input input-stream
                                      :output :stream
                                      :wait nil)))
    (format nil "~{~A~^~%~}"  (loop for line = (read-line (process-output process) nil :eof)
                                 until (eq line :eof)
                                 collect line))))

(defun goo-search (word)
  (plump:parse
   (drakma:http-request (concatenate 'string "https://dictionary.goo.ne.jp/srch/jn/" (do-urlencode:urlencode word) "/m0u/"))))

(defmethod goo-word-fill ((word goo-word))
  (let ((page (goo-search (goo-word-reading word))))
    (cond ((page-is-multiple page)
	   (progn
	     (setf (slot-value word 'results-page) page)
	     (setf (slot-value word 'entry-page) (page-select-result page))))
	  ((page-is-single page)
	   (progn
	     (setf (slot-value word 'entry-page) page)))
	  (t
	   (error "That page didn't match any of the conditions! I don't know what to do")))))


(defun lookup-new-word (word)
  (let ((new-word (make-instance 'goo-word :reading word)))
    (setf (gethash word *words*) new-word)
    (goo-word-fill new-word)))

(defun plump-text-without-comments (node)
  "This is a copy of plump's text method without adding comments"
  (with-output-to-string (stream)
    (labels ((r (node)
               (loop for child across (plump::children node)
		  do (cond ((and (typep child 'plump::textual-node) (not (typep child 'plump::comment))) (write-string (plump::text child) stream)) 
			   ((typep child 'plump::nesting-node) (r child))))))
      (r node))))

(defun simple-text-print (word)
  (string-trim '(#\Space #\Newline)
	       (cl-ppcre:regex-replace-all "(?m)\\n{3,}"
					   (plump-text-without-comments (entry-contents (page-entry (slot-value word 'entry-page)))) (format nil "~%~%"))))


(defun lookup-and-show-new-word (word &key parent-word parent-sentence)
  (let ((new-word (make-instance 'goo-word :reading word)))

    (setf (gethash word *words*) new-word)
    (goo-word-fill new-word)

    (if parent-word
        (progn
          (setf (goo-word-children (gethash parent-word *words*)) (cons new-word (goo-word-children (gethash parent-word *words*))))
          (setf (goo-word-parents new-word) (cons (gethash parent-word *words*) (goo-word-parents new-word)))))

    (if parent-sentence
        (let ((parent-sentence (sentence-from-string parent-sentence)))
          (progn
            (setf (goo-sentence-children parent-sentence)
                  (cons new-word (goo-sentence-children parent-sentence))))))

    (swank::eval-in-emacs
     `(show-goo-word
       ,word
       ,(simple-text-print new-word)))))

;; kotowaza section (for example
;; https://dictionary.goo.ne.jp/jn/31936/meaning/m0u/%E7%94%B7%E3%82%92%E7%A3%A8%E3%81%8F/)
;; might cause trouble still

;; (let ((node (goo::entry-contents (goo::page-entry (slot-value (gethash "大統領" goo:*words*) 'goo::entry-page)))))

;;   (with-output-to-string (stream)
;;     (labels ((r (node)
;;                (loop for child across (plump::children node)
;; 		  do (cond ((and (typep child 'plump::textual-node) (not (typep child 'plump::comment))) (write-string (plump::text child) stream)) 
;; 			   ((typep child 'plump::nesting-node) (r child))))))
;;       (r node))))
