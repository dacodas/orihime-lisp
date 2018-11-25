(in-package :orihime)

(log:config :debug)

(defvar *words* (make-hash-table :test #'equal))
(defvar *texts* (make-hash-table :test #'equalp))
(defvar *definitions* (make-array 10 :adjustable t :fill-pointer 0))
(defparameter *current-backend* :goo-local)
(defparameter *mongo-database* "orihime")
(cl-mongo:db.use *mongo-database*)

;; Definition can either be plain text or some sort of markup like
;; HTML or markdown
(defclass text ()
  ((contents
    :initarg :contents
    :initform nil
    :reader text-contents
    :type (simple-array character))
   (child-words
    :initarg :child-words
    :initform (make-array 10 :element-type 'child-word :adjustable t :fill-pointer 0)
    :reader text-child-words
    :type (array child-word-in-context))
   (id
    :initarg :id
    :reader text-id
    :type (simple-array character))
   (text-type
    :reader text-type
    :initarg :text-type
    :initform :unspecified)))

(defun get-text-peek (text)
  (format nil "~A..."
          (let* ((contents (text-contents text))
                 (peek (subseq contents 0 (min 10 (length contents)))))
            (trim-and-replace-big-breaks peek))))

(defun get-text-child-words-peek (text)
  (let ((child-words (text-child-words text)))
    (loop for index below (min 5 (length child-words))
       collect (word-reading (aref child-words index)))))

(defun print-object-texty-boy (text stream)
  (format stream "#<GOO-TEXT ~S \"~A\" ~{~A~^, ~}>"
          (text-type text)
          (get-text-peek text)
          (get-text-child-words-peek text)))

(defclass definition-text (text)
  ((type :initform :definition)))

(defclass word-to-study ()
  ((word-reading
    :initarg :word-reading
    :accessor word-reading
    :type (simple-array character))
   (word-type
    :initarg :word-type
    :initform :unspecified)
   (definition
       :initarg :definition
     :accessor word-definition
     :initform nil
     :type definition-text)))

;; These are meant to form the vector of child words within a sentence
;; or word-to-study. Meant to be shallow, ID's rather than substantial
;; objects
(defclass child-word-in-context ()
  ((word-reading
    :initarg :word-reading
    :reader word-reading
    :type (simple-array character))
   (start
    :initarg :start
    :type integer)
   (end
    :initarg :end
    :type integer)))

(defmethod print-object ((word child-word-in-context) stream)
  (format stream "#<CHILD-WORD ~A ~A ~A>"
          (word-reading word)
          (slot-value word 'start)
          (slot-value word 'end)))

(defun child-words-list-p (vector)
  (and (typep vector 'vector)
       (every (lambda (el) (subtypep (type-of el) 'child-word)) vector)))

(defun make-definition (contents)
  (let ((definition (make-text contents :text-type :definition)))
    (vector-push-extend definition *definitions* 10)
    definition))

(defun make-text (contents &key (text-type :unspecified))
  (let* ((hash (cl-base64:usb8-array-to-base64-string (ironclad:digest-sequence :sha256 (sb-ext:string-to-octets contents))))
         (this-text (make-instance 'text :contents contents :id hash :text-type text-type)))
    (setf (gethash hash *texts*) this-text)))

(defun get-text-from-id (id)
  (text-contents (gethash id *texts*)))

(defun word-definition-text-id (reading)
  (word-definition (search-for-word reading)))

(defun add-child-word-to-child-words (child-word child-words)

  (let ((index-of-already-present-child-word (loop for current-child-word across child-words
                                                for index below (length child-words)
                                                do (if (equal (word-reading child-word) (word-reading current-child-word))
                                                       (return index)))))

    (if index-of-already-present-child-word
        (setf (aref child-words index-of-already-present-child-word) child-word)
        (vector-push-extend child-word child-words 10))))

(defun add-child-word-to-text (text-id reading ocurrence-in-text)
  (let ((this-word (search-for-word reading))
        (this-text (gethash text-id *texts*)))
    (multiple-value-bind (start end)
        (cl-ppcre:scan ocurrence-in-text (text-contents this-text))
      (let ((child-word (make-instance 'child-word-in-context
                                        :word-reading reading
                                        :start start
                                        :end end)))

        (add-child-word-to-child-words child-word
                                       (slot-value this-text 'child-words))
        t))))

(defun find-definition-from-backend (reading)
  (case *current-backend*
    (:goo-web (goo-definition-from-meaning-page (goo-web-search reading)))
    (:goo-local (goo-definition-from-meaning-page (goo-local-search reading)))))

(defun add-word (reading)
  (let* ((definition-text-string (find-definition-from-backend reading))
         (definition (make-definition definition-text-string))
         (definition-id (text-id definition))
         (word (make-instance 'word-to-study
                              :word-reading reading
                              :definition definition-id)))
    (setf (gethash reading *words*) word)))

(defun search-for-word (reading)
  (let ((hash-result (gethash reading *words*)))
    (if hash-result
        hash-result
        (add-word reading))))
