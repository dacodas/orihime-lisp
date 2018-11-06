(in-package :goo)

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
  (let* ((contents (text-contents text))
         (peek (subseq contents 0 (min 10 (length contents)))))
    (trim-and-replace-big-breaks peek)))

(defun get-text-child-words-peek (text)
  (let ((child-words (text-child-words text)))
    (loop for index below (min 5 (length child-words))
       collect (word-reading (aref child-words index)))))

(defun print-object-texty-boy (text stream)
  (format stream "#<GOO-TEXT ~S \"~A...\" ~{~A~^, ~}>"
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

(defun child-words-list-p (vector)
  (and (typep vector 'vector)
       (every (lambda (el) (subtypep (type-of el) 'child-word)) vector)))

(defclass goo-search ()
  ((search-term
    :initarg :search-term)
   (results 
    :initarg :results
    :reader goo-search-results)
   (result-type
    :initarg :result-type
    :reader goo-result-type)))

(defclass goo-entry ()
  ((entry-number
    :initarg :entry-number
    :reader goo-word-entry-number)
   (entry ;; This would be the response of the entry request
    :initarg :entry
    :reader goo-word-entry)))

(defclass goo-word-to-study (word-to-study)
  ((goo-search
    :reader goo-search)
   (goo-entry
    :reader goo-entry)
   (word-type :initform :goo-word)))
