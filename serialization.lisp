(in-package :orihime)

;; These are taken right out of cl-mongo, but I've replace the ~A
;; format directives with ~S instead so it's a bit clearer for me

#+nil(defmethod print-object.dacoda ((document document) stream)
  (format stream "<~S> : { ~%" (type-of document))
  (when (and (slot-boundp  document '_local_id) (slot-boundp  document '_id))
    (unless (_local document) (format stream "~3,8T_id : ~S~%" (_id document))))
  (if (slot-boundp document 'elements)
      (progn
	(format stream "~3,8Telements : ~S" (hash-table-count (elements document)))
	(print-hash.dacoda (elements document) stream :max 20))
      "no elements set..")
  (format stream "}~%"))

#+nil(defun print-hash.dacoda (ht stream &key (max (hash-table-count ht)))
  (labels ((prdocs (docs)
	     (format stream "~1,1T[")
	     (block print-array
	       (let ((counter 0))
		 (dolist (doc docs)
		   (incf counter)
		   (if (typep doc 'document)
		       (print-hash.dacoda (elements doc) stream :max max)
		       (format stream "~S," doc))
		   (when (> counter 100)
		     (progn
		       (format stream "[....](~A elements left)" (- (length docs) 100))
		       (return-from print-array nil))))))
	     (format stream "]~%"))
	   (vpr (v)
	     (cond ((typep v 'cons)     (prdocs v))
		   ((typep v 'document) (prdocs (list v)))
		   (t                  (format stream "~S~%" v)))))
    (format stream "~%~3,8T{~%")
    (with-hash-table-iterator (iterator ht)
      (dotimes (repeat max)
	(multiple-value-bind (exists-p key value) (iterator)
	  (if exists-p 
	      (progn 
		(format stream "~3,8T~S -> " key)
		(vpr value))))))
    (when (< max (hash-table-count ht)) (format stream "~3,8T[..............]~%"))
    (format stream "~3,8T}~%")))

(defmacro with-mongo-database ((database-name) &body body)
  (if (equal *mongo-database* database-name)
      `(,@body)
      `(let ((previous-database ,*mongo-database*)
             (*mongo-database* ,database-name))
         (unwind-protect
              (progn
                (cl-mongo:db.use *mongo-database*)
                ,@body)
           (cl-mongo:db.use previous-database)))))

(defun make-document-from-class (object &optional (oid nil))
  (let ((document (cl-mongo:make-document :oid oid))
        (class (class-of object)))
    (loop for slot in (sb-mop:class-slots class)
         do (let ((slot-name (sb-mop:slot-definition-name slot)))
           (cl-mongo:add-element (string-downcase (symbol-name slot-name))
                                 (serialize-for-mongo (slot-value object slot-name))
                                 document)))
    document))

(defun serialize-for-mongo (object)
  (etypecase object
    (number object)
    (string object)
    (symbol (string-downcase (symbol-name object)))
    (puri:uri              (fix-puri-uri object))
    (lparallel.promise::%future (serialize-for-mongo (lparallel:force object)))
    (text                  (make-document-from-class object (text-id object)))
    (child-word-in-context (make-document-from-class object (word-reading object)))
    (word-to-study         (make-document-from-class object (word-reading object)))
    (vector (loop for element across object
               collect (serialize-for-mongo element)))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; I think the rest after this are kind of sketchy
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    ;; This probably came from the stream result of a drakma:http-request 
    (flexi-streams:flexi-io-stream nil)

    ;; This has to handle the alist coming as the headers of the drakma:http-request
    (cons (if (not (typep (cdr object) 'cons))
              (list (serialize-for-mongo (car object))
                    (serialize-for-mongo (cdr object)))
              (loop for sub-object in object 
                   collect (serialize-for-mongo sub-object))))))


(defun persist ()
  (with-mongo-database (*mongo-database*)
    (loop for k being the hash-keys of *words*
       using (hash-value value)
       do (cl-mongo:db.save "words" (serialize-for-mongo value)))
    (loop for k being the hash-keys of *texts*
       using (hash-value value)
       do (cl-mongo:db.save "texts" (serialize-for-mongo value)))))

;;;;;;;;;;;;;;;;;;;;;
;; Deserialization ;;
;;;;;;;;;;;;;;;;;;;;;

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
  (with-mongo-database (*mongo-database*)
    (if class-name
        (let ((class-slots (get-class-slots class-name))
              (new-object (make-object-from-class-name class-name)))
          (log:debug "Here are the class slots: ~S" class-slots)
          (log:debug "Here are the keys: ~{~S~^, ~}" (cl-mongo:get-keys document))
          (loop for key in (cl-mongo:get-keys document)
             do (let* ((slot-name (intern (string-upcase key) :orihime))
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
                    (print-object document stream)
                    (get-output-stream-string stream))))))))

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
