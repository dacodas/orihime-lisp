(in-package :orihime)

(lquery:define-lquery-function text-without-comments (node)
  (with-output-to-string (stream)
    (labels ((r (node)
               (loop for child across (plump::children node)
		          do (cond ((and (typep child 'plump::textual-node) (not (typep child 'plump::comment))) (write-string (plump::text child) stream)) 
			               ((typep child 'plump::nesting-node) (r child))))))
      (r node))))

(export 'lquery-funcs::text-without-comments :lquery-funcs)

(defun trim-and-replace-big-breaks (text)
  (let* ((big-breaks-removed (cl-ppcre:regex-replace-all "(?m)\\n{3,}" text (format nil "~%~%")))
         (trimmed (string-trim '(#\Space #\Newline) big-breaks-removed)))
    trimmed))

(defun goo-definition-from-meaning-page (body)
  (let* ((title (lquery:$1 (initialize body)
                           "div.basic_title h1" (text-without-comments)))
         (definition-ugly (lquery:$1 (initialize body)
                                     "div.meaning_area div.contents" (text-without-comments)))
         (definition-pretty (trim-and-replace-big-breaks definition-ugly)))
    (format nil "~A~%~%~A" title definition-pretty)))

