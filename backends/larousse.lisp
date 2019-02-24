(in-package :orihime)

(defun larousse-search (word)
  (let* ((response (drakma:http-request (concatenate 'string "https://larousse.fr/dictionnaires/francais/" (do-urlencode:urlencode word))
                                        :user-agent *user-agent*))
         (text (lquery:$1 (initialize response)
                          "ul.Definitions"
                          (text))))
    
    (with-output-to-string (output)
      (with-input-from-string (stream text)
        (loop for line = (read-line stream nil 'eof)
           while (not (eq line 'eof))
           do
             (let ((trimmed (string-trim '(#\Space #\Tab #\Return) line)))
               (if (> (length trimmed) 0)
                   (format output "~A~%~%" trimmed))))))))
