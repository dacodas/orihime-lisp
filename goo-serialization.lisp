(in-package :goo)

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
    (goo-search            (make-document-from-class object))
    (goo-entry             (make-document-from-class object))
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
  (cl-mongo:db.use "totally-new")
  (loop for k being the hash-keys of *words*
     using (hash-value value)
     do (cl-mongo:db.save "words" (serialize-for-mongo value)))
  (loop for k being the hash-keys of *texts*
     using (hash-value value)
     do (cl-mongo:db.save "texts" (serialize-for-mongo value))))
