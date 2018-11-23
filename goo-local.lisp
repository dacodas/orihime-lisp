(in-package :goo)

(defparameter *goo-trie-socket* (usocket:socket-connect "localhost" 7081))
(defparameter *goo-local-cache* #P"/home/dacoda/projects/goo-processing/dictionary-entries/")

(defun lookup-word-local (reading)

  (let ((stream (usocket:socket-stream *goo-trie-socket*)))
    (format stream "LEMMEKNOW ~A" reading)
    (force-output stream))

  (sleep 0.1)

  ;; Handle the error here I get when the UTF-8 character ends in the middle
  (let ((results (let ((input-buffer (make-array 1048576 :element-type '(unsigned-byte 8))))
                   (multiple-value-bind (buffer length peer-address)
                       (sb-bsd-sockets:socket-receive (usocket:socket *goo-trie-socket*) input-buffer nil)
                     (read-from-string (sb-ext:octets-to-string buffer))))))
    results))

(defun local-results-paging (results page-number &key (results-per-page 10))
  (let ((starting-value (* page-number results-per-page)))
    (loop for i from starting-value below (min (+ starting-value results-per-page)
                                               (- (length results) starting-value)) 

       do (let* ((result (aref results i))
                 (entry-file-name (aref result 1)))

            (with-open-file (entry-file (merge-pathnames (make-pathname :name (format nil "~A" entry-file-name) :type "html")
                                                         *goo-local-cache*))
              (let ((entry-file-contents (make-array (file-length entry-file) :element-type 'character)))
                (read-sequence entry-file-contents entry-file :end (file-length entry-file))
                (let* ((definition (definition-from-goo-meaning-page entry-file-contents))
                       (peek (subseq definition 0 (min 20 (length definition))))))
                (format t "~A: ~A...~%"
                        i
                        (let* ((definition (definition-from-goo-meaning-page entry-file-contents))
                               (peek (subseq definition 0 (min 20 (length definition)))))
                          (cl-ppcre:regex-replace-all "(?m)\\n" peek (format nil ""))))))))

    (format t "~%Enter a number to access the definition, 'n' to go to the next page, and 'p' to go the previous page:~%")
    (let ((user-input (read-line))
          (parsed-integer (handler-case (parse-integer user-input)
                            (error (c) nil))))
      (cond ((equal user-input "n") (local-results-paging results (1+ page-number) :results-per-page results-per-page))
            ((equal user-input "p") (local-results-paging results (1- page-number) :results-per-page results-per-page))
            (t (handler-case (let ((parsed-integer (parse-integer user-input)))
                               (if (and (>= parsed-integer 0) (< parsed-integer (length list-titles-and-mean-texts)))
                                   (progn
                                     (format t "Going to selection ~A~%" parsed-integer)
                                     (grab-goo-relative-page (third (aref list-titles-and-mean-texts parsed-integer))))
                                   (progn
                                     (format t "That number is too large or too small~%")
                                     (local-results-paging results page-number :results-per-page results-per-page)))) 
                 (error (simple-parse-error)
                   (progn
                     (format t "That isn't a valid input~%")
                     (local-results-paging results page-number :results-per-page results-per-page)))))))
    ))


