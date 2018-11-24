(in-package :orihime)

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

(defun get-word-definition-id (reading)
  (word-definition (grab-or-make-word reading)))

(defun add-child-word-to-child-words (child-word child-words)

  (let ((index-of-already-present-child-word (loop for current-child-word across child-words
                                                for index below (length child-words)
                                                do (if (equal (word-reading child-word) (word-reading current-child-word))
                                                       (return index)))))

    (if index-of-already-present-child-word
        (setf (aref child-words index-of-already-present-child-word) child-word)
        (vector-push-extend child-word child-words 10))))

(defun add-child-word-to-text (text-id reading ocurrence-in-text)
  (let ((this-word (grab-or-make-word reading))
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

;; Modify this so that it can use whatever backend you need
(defun grab-or-make-word (reading &optional (fill t))
  (let ((hash-result (gethash reading *words*)))
    (if hash-result 
        hash-result
        (let ((new-word (make-goo-word-to-study reading)))
          (if fill (fill-goo-word-to-study new-word))










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
          new-word))))
