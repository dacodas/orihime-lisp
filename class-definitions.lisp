;; Definition can either be plain text or some sort of markup like
;; HTML or markdown
(defclass text ()
  (text)
  (child-words
   :initarg child-words
   :initform nil))

(defclass word ()
  ((reading
    :initarg :reading
    :accessor word-reading)
   (definition
     :initarg :definition
     :accessor word-definition)))

(defclass child-word ()
  ((reading
     :initarg :word-reading)))

(defclass child-word-in-context (child-word)
  ((position
    :initarg :position)
   (length
    :initarg :length)))

(defun child-words-list-p (vector)
  (and (typep vector 'vector)
       (every (lambda (el) (subtypep (type-of el) 'child-word)) vector)))
