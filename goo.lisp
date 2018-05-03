(load "/home/dacoda/quicklisp/setup.lisp")

(ql:quickload '(:drakma :do-urlencode :plump :lquery :log4cl))

(log:config :debug)

(defun strip (string)
  (string-trim '(#\Space #\Tab #\Newline #\Linefeed #\Rubout #\Return) string))

(defun page-navbar (page)
  "Take in a goo page and return the navbar describing where in the dictionary you are"
  (lquery:$1 page "div#NR-wrapper nav"))

(defun page-entry (page)
  (lquery:$1 page "div#NR-main div#NR-main-in section div.section"))

(defun navbar-to-list (navbar)
  (map 'list
       (lambda (navbar-item) (strip (lquery-funcs:text navbar-item)))
       (lquery:$ navbar "ol li")))

(defun entry-title (entry)
  (lquery:$1 entry "div.basic_title.nolink.jn h1"))

(defun entry-contents (entry)
  (lquery:$1 entry "div.contents_area div.contents"))

(defun title-match-readings (title)
  (cl-ppcre:register-groups-bind
      (readings nil old-readings word)
      ("(.*?)(〔(.*?)〕)?【(.*?)】" (lquery-funcs:text title))
    (values-list (list word readings old-readings))))

(defun format-html-as-text (string-to-format)
  (let* ((input-stream (make-string-input-stream string-to-format))
         (process (sb-ext:run-program "/usr/bin/html2text" '("--ignore-links")
                                      :input input-stream
                                      :output :stream
                                      :wait nil)))
    (format nil "~{~A~^~%~}"  (loop for line = (read-line (process-output process) nil :eof)
                                 until (eq line :eof)
                                 collect line))))

(defun page-html-title-regex-matcher (regex page)
  (let ((page-html-title (lquery:$1 page "head title" (lquery-funcs:text))))
    (cl-ppcre:scan regex page-html-title)))

(defun page-is-single (page)
  (page-html-title-regex-matcher ".*の意味.*" page))

(defun page-is-multiple (page)
  (page-html-title-regex-matcher ".*の検索結果.*" page))

(defun page-select-result (page)
  (if (not (page-is-multiple page))
      (error "This page was called for select-result but doesn't have multiple results!"))
  (princ (format nil "~a~%~%" "Select from the following results:"))
  (let ((links-and-texts (lquery:$ page ".section .search ul li a"
                                   (lquery-macros:combine (lquery-funcs:attr :href) (lquery:$1 "dt.title" (lquery-funcs:text))))))
    (map nil
         (lambda (entry number) (princ (format nil "~a ~a~%" number (cadr entry))))
         links-and-texts
         (loop for i from 1 to (length links-and-texts) collect i))
    (let* ((user-selection (parse-integer (read-line)))
           (relative-link (car (aref links-and-texts (- user-selection 1))))
           (absolute-link (format nil "~a~a" "https://dictionary.goo.ne.jp" relative-link)))
      (log:debug "Grabbing the following page and returning: ~a" absolute-link)
      (plump:parse (drakma:http-request absolute-link)))))

(defun page-show-result (page)
  (if (not (page-is-single page))
      (error "This page was called for show result but it doesn't have just one result!"))
  (let* ((entry (page-entry page))
         (title (entry-title entry))
         (contents (entry-contents entry)))
    ;; (log:debug "Got the following entry: ~a" (lquery-funcs:serialize entry))
    (log:debug "Got the following title: ~a" (lquery-funcs:text title))
    (log:debug "Got the following contents: ~a" (lquery-funcs:text contents))
    (log:debug "Here's the navbar: ~a" (navbar-to-list (page-navbar page)))
    (format t (format-html-as-text (lquery-funcs:serialize contents)))))

;; How can I combine the below two macros into a single macro? 
(defmacro test-with-randoms-and-collect (number-of-times &body body)
  `(loop for i below ,number-of-times collect
       (let* ((random-number (random 299999 (make-random-state t)))
            (url (concatenate 'string "https://dictionary.goo.ne.jp/jn/" (write-to-string random-number) "/meaning/m0u/"))
            (page (plump:parse (drakma:http-request url))))
       (log:debug "Testing with ~a" (write-to-string random-number))
       ,@body
       ))
  )

(defmacro test-with-randoms (&body body)
  `(dotimes (i 10)
     (let* ((random-number (random 299999 (make-random-state t)))
            (url (concatenate 'string "https://dictionary.goo.ne.jp/jn/" (write-to-string random-number) "/meaning/m0u/"))
            (page (plump:parse (drakma:http-request url))))
       (log:debug "Testing with ~a" (write-to-string random-number))
       ,@body
       )))

(defun goo-search (word)
  (plump:parse
   (drakma:http-request (concatenate 'string "https://dictionary.goo.ne.jp/srch/jn/" (do-urlencode:urlencode word) "/m0u/"))))

;; Test entry parsing with random pages


(defun page-log-information (page)
  (let* ((entry (page-entry page))
         (title (entry-title entry))
         (contents-html (lquery-funcs:serialize (entry-contents entry))))

    (multiple-value-bind (word readings old-readings)
        (title-match-readings title)
      (log:debug "Path to this word: ~a" (format nil "~{~a~^ → ~}" (navbar-to-list (page-navbar page))))
      (log:debug "Word: ~a (~a) ~a" word readings (if old-readings (format nil "(~a)" old-readings) ""))
      (log:debug "Contents through html2text: ~a" (format-html-as-text contents-html))
      ;; (log:debug "Contents (raw html): ~a" contents-html)
      )))

(defun element-tag (element)
  (intern (nth 1 (split-sequence:SPLIT-SEQUENCE #\Space (format nil "~A" element)))))

(defun get-skeleton (element)
  (let ((children (lquery-funcs:children element)))
    (if (equalp children (vector))
        (progn
	  (log:debug "No more children!")
	   element)
        (progn
	  (log:debug "We're going another level!")
	  (log:debug children)
	  (cons element
		(map 'list
		     (lambda (child) (get-skeleton child))
		     children))))))

(defun skeleton-just-element-tags (skeleton)
  (cond ((eq nil skeleton) nil)
	((listp skeleton) (cons (skeleton-just-element-tags (car skeleton)) (skeleton-just-element-tags (cdr skeleton)))) 
	(t (element-tag skeleton))))

(defun skeleton-has-only-text-elements (skeleton)
  (cond ((eq nil skeleton) t)
        ((listp skeleton) (and (skeleton-has-only-text-elements (car skeleton))
                               (skeleton-has-only-text-elements (cdr skeleton))))
        (t (if (position (element-tag skeleton) '(|p| |div| |a| |strong| |span| |div|)) t))))

(defun skeleton-replace-with-text (skeleton)
  (cond ((listp skeleton) (cons (car skeleton) (mapcar #'skeleton-replace-with-text (cdr skeleton))))
        ((skeleton-has-only-text-elements skeleton) (lquery-funcs:text skeleton))
        (t skeleton)))

(defun call-dot (dot-pathname pdf-pathname)
  (sb-ext:run-program "/usr/sbin/dot" (list "-Tpdf" (namestring dot-pathname) "-o" (namestring pdf-pathname))))


(defun contents->graphviz (contents &key title)

  (let ((elements-node-map (make-hash-table :test #'eq))
        (node-count 0)
        (output-stream (make-string-output-stream)))

    (defun make-nodes (element)
      (if (eq nil element)
          nil
          (progn
            (log:debug "This element is ~A and has class ~A and will be assigned node ~A" element (class-of element) node-count)

            (setf (gethash element elements-node-map) node-count)

            (cond ((plump:text-node-p element) (progn (format output-stream "~A [label=\"~A\"];~%" node-count "text") (incf node-count)))
                  ((typep element 'plump:comment )(progn (format output-stream "~A [label=\"~A\"];~%" node-count "comment") (incf node-count)))
                  (t (progn (format output-stream "~A [label=\"~A\"];~%" node-count (element-tag element))
                            (incf node-count)
                            (map 'nil #'make-nodes (plump:children element)))))))

      )

    (defun connect-nodes (element)

      (if (eq nil element)
          nil
          (progn

            (let ((node (gethash element elements-node-map)))

              (log:debug "Working with element ~A of class ~A and node value ~A" element (class-of element) node)

              (cond ((plump:text-node-p element) (log:debug "This is a text node (and thus has no children)..."))
                    ((typep element 'plump:comment (log:debug "This is a comment (and has no children)")))
                    ((equalp (vector) (plump:children element)) (log:debug "This is an element, but it has no children..."))
                    (t (progn
                         (let ((children-nodes (plump:children element)))
                           (log:debug "This has children... it is node ~A and is attached to ~{~A~^, ~}" node (map 'list (lambda (child) (gethash child elements-node-map)) children-nodes))
                           (map nil (lambda (child) (format output-stream "~A -> ~A;~%" node (gethash child elements-node-map))) children-nodes)
                           (map 'vector #'connect-nodes (plump:children element)))))))))
      t)

    (format output-stream "~A~%" "digraph BST {")
    (if title (format output-stream "graph [label=\"~A\", labelloc=top];~%" title))
    (format output-stream "~A~%" "node [fontname=\"Arial\"];")
    (make-nodes contents)
    (connect-nodes contents)
    (format output-stream "~A~%" "}")
    (get-output-stream-string output-stream)))

(defun contents->pdf (contents pdf-pathname &key title)
  (let ((dot-pathname (make-pathname :defaults pdf-pathname :type "dot")))
    (with-open-file (output-file-stream dot-pathname
					:direction :output
					:if-exists :supersede
					:if-does-not-exist :create)
      (format output-file-stream (contents->graphviz contents :title title)))
    (call-dot dot-pathname pdf-pathname)
     pdf-pathname))

(defun pdfunite (output-file input-files)
  (sb-ext:run-program "/usr/sbin/pdfunite" 
		      (concatenate 'list (mapcar #'namestring input-files) (list (namestring output-file)))))

;; (time (pdfunite (make-pathname :directory '(:absolute "tmp") :name "composite" :type "pdf")
;;                 (mapcar (lambda (page count)
;;                           (let ((file-pathname (make-pathname
;;                                                 :directory '(:absolute "tmp")
;;                                                 :name (format nil "skeleton.~A" count)
;;                                                 :type "pdf")))
;;                             (contents->pdf (entry-contents (page-entry page)) file-pathname
;;                                            :title (format nil "~A\\n~A" (format nil "~{~a~^ → ~}" (navbar-to-list (page-navbar page))) (title-prettify (entry-title (page-entry page))))))) 
;;                         *test-pages*
;;                         (loop for i from 1 upto (length *test-pages*) collect i))))



(defun page-title (page)
  (entry-title (page-entry page)))

(defun title-prettify (title)
  (if (eq nil title)
      "This title is bunk as fuck, bro"
      (multiple-value-bind (word readings old-readings) (title-match-readings title)
        (format nil
                "~A (~A) ~@[[~A]~]"
                word
                readings
                old-readings))))

;; https://stackoverflow.com/questions/19775005/speeding-up-deleting-duplicates-when-theyre-adjacent
(defun delete-adjacent-duplicates (list &key key (test 'eql))
  (loop
     for head = list then (cdr head)
     until (endp head)
     finally (return list)
     do (setf (cdr head)
              (member (if (null key) (car head)
                          (funcall key (car head)))
                      (cdr head)
                      :key key :test-not test))))

(defvar *test-pages*
  (test-with-randoms-and-collect 20
    page))

(defclass jap-word ()
  ((reading
    :initarg :reading
    :accessor jap-word-reading)
   (results-page
    :initform nil)
   (entry-page
    :initform nil)
   (linked-words
    :initform nil)))

(defmethod fill-from-goo ((word jap-word))
  (let ((page (goo-search (jap-word-reading word))))
    (cond ((page-is-multiple page)
	   (progn
	     (setf (slot-value word 'results-page) page)
	     (setf (slot-value word 'entry-page) (page-select-result page))))
	  ((page-is-single page)
	   (progn
	     (setf (slot-value word 'entry-page) page)))
	  (t
	   (error "That page didn't match any of the conditions! I don't know what to do")))))

(defvar *words* 
    (make-hash-table :test #'equal))

(defun lookup-new-word (word)
  (let ((new-word (make-instance 'jap-word :reading word)))
    (setf (gethash word *words*) new-word)
    (fill-from-goo new-word)))

(defun simple-text-print (word)
  (string-trim '(#\Space #\Newline)
	       (cl-ppcre:regex-replace-all "(?m)\\n{3,}"
					   (lquery-funcs:text (entry-contents (page-entry (slot-value word 'entry-page)))) (format nil "~%~%"))))

(defun lookup-and-show-new-word (word)
  (let ((new-word (make-instance 'jap-word :reading word)))
    (setf (gethash word *words*) new-word)
    (fill-from-goo new-word)
    (swank::eval-in-emacs
     `(show-japanese-word
       ,word
       ,(simple-text-print new-word)))))

;; kotowaza section (for example
;; https://dictionary.goo.ne.jp/jn/31936/meaning/m0u/%E7%94%B7%E3%82%92%E7%A3%A8%E3%81%8F/)
;; might cause trouble still


























;; Testing lexing...
(defun munch-mecab-output (line-of-mecab-output)
  (let* ((first-split (split-sequence:SPLIT-SEQUENCE #\Tab line-of-mecab-output))
         (name (car first-split))
         (fields (split-sequence:SPLIT-SEQUENCE #\, (cadr first-split))))
    (concatenate 'list (list name) fields)))

(defun lex-text (text-to-lex)
  (let* ((input-stream (make-string-input-stream text-to-lex))
         (process (sb-ext:run-program "/usr/bin/mecab" '()
                                      :input input-stream
                                      :output :stream
                                      :wait nil)))
      (loop for line = (read-line (process-output process) nil :eof)
                                  until (eq line :eof)
         collect (munch-mecab-output line))))

;; (loop for token in
;;       (lex-text "［名］(スル)銃砲を発射するとき、火薬ガスの圧力が、弾丸を腔内から発射させると同時に、銃砲そのものを後方へ押しやること。")
;;       when (not (member (cadr token) '("記号" "助詞") :test #'equal))
;;       collect token
;;       )
