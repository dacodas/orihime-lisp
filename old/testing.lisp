(use-package :cl-mongo)

(cl-mongo:db.use "goo")

(format t "~S"  )

(caadr )
(loop for document in (cadr (cl-mongo:iter (cl-mongo:db.find "words" :all)))
   collect ;; (maphash (lambda (k v) (format t "Here: ~a~%" k)) (cl-mongo::elements document))
     (format t "~%NEW WORD~%~A:~%~A"
             (cl-mongo::_id document)
             (string-trim '(#\Space #\Newline)
	                      (cl-ppcre:regex-replace-all "(?m)\\n{3,}"
					                                  (goo::plump-text-without-comments (goo::entry-contents (goo::page-entry (plump:parse (cl-mongo:get-element "entry-page" document))))) (format nil "~%~%")))))
