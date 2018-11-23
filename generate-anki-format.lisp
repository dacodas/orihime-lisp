(in-package :orihime)

(cl-mongo:db.use "totally-new")

;; Don't work with symbols. Instead, make-instance and then populate
;; the slots. 

(defun get-class-slots (class-name)
  (loop for slot in (sb-mop:class-direct-slots (find-class class-name))
     collect (sb-mop:slot-definition-name slot)))

(defmacro make-object-from-class-name (class-name)
  `(make-instance ,class-name))

(defun handle-slot-value (slot-name slot-value)
  ;; (log:debug "Handling slot name ~s with value ~S" slot-name slot-value)
  (case slot-name
    ('child-words (let* ((child-words-length (length slot-value))
                         (new-child-words (make-array child-words-length)))
                    (loop for child-document in slot-value
                       for index below child-words-length
                       do (setf (aref new-child-words index) (unserialize-mongo-document child-document 'child-word-in-context)))
                    new-child-words))
    ('text-type (find-symbol (string-upcase slot-value) 'keyword))
    ('goo-search (unserialize-mongo-document slot-value 'goo-search))
    ('goo-entry (unserialize-mongo-document slot-value 'goo-entry))
    (t (unserialize-mongo-document slot-value))))

(defun unserialize-mongo-document (document &optional class-name)
  (if class-name
      (let ((class-slots (get-class-slots class-name))
            (new-object (make-object-from-class-name class-name)))
        (log:debug "Here are the class slots: ~S" class-slots)
        (log:debug "Here are the keys: ~{~S~^, ~}" (cl-mongo:get-keys document))
        (loop for key in (cl-mongo:get-keys document)
           do (let* ((slot-name (intern (string-upcase key) :goo))
                     (value (handle-slot-value slot-name (cl-mongo:get-element key document))))
                (labels ((slot-is-valid ()
                           (and (not (null value)) (find slot-name class-slots))))
                  (log:debug "Slot ~S is ~Avalid" slot-name (if (slot-is-valid) "" "not "))
                  (if (slot-is-valid)
                   (setf (slot-value new-object slot-name) value)))))
        new-object)

      (etypecase document
        (number document)
        (string document)
        (null nil)
        (cons (mapcar #'unserialize-mongo-document document))
        (cl-mongo:document
         (error "You want me to unserialize a mongo document, but you didn't tell me how to unserialize it: ~%~A"
                (let ((stream (make-string-output-stream)))
                  (cl-mongo::print-object.dacoda document stream)
                  (get-output-stream-string stream)))))))

(defun unserialize-texts ()
  (mapcar (lambda (document)
            (let ((new-text (unserialize-mongo-document document 'text)))
              (setf (gethash (text-id new-text) *texts*) new-text)))
          (second (cl-mongo:iter (cl-mongo:db.find "texts" :all)))))

(defun unserialize-words ()
  (mapcar (lambda (document)
            (let ((new-word (unserialize-mongo-document document 'word-to-study)))
              (setf (gethash (word-reading new-word) *words*) new-word)))
          (second (cl-mongo:iter (cl-mongo:db.find "words" :all)))))
