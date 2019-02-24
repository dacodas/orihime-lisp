(in-package :orihime)

(defparameter *goo-host* "goo-local-container")
(defparameter *goo-port* 7081)
(defparameter *goo-local-cache* #P"/var/lib/goo/data/dictionary-entries/")

(defparameter *goo-trie-socket* nil)

(defun goo-local-search (reading)

  (if (not *goo-trie-socket*)
      (setf *goo-trie-socket* (usocket:socket-connect *goo-host* *goo-port*)))

  (let ((stream (usocket:socket-stream *goo-trie-socket*)))
    (format stream "LEMMEKNOW ~A" reading)
    (force-output stream))

  (sleep 0.1)

  ;; Handle the error here I get when the UTF-8 character ends in the middle
  (let ((results (let ((input-buffer (make-array 1048576 :element-type '(unsigned-byte 8))))
                   (multiple-value-bind (buffer length peer-address)
                       (sb-bsd-sockets:socket-receive (usocket:socket *goo-trie-socket*) input-buffer nil)
                     (read-from-string (sb-ext:octets-to-string buffer))))))
    (results-paging (make-instance 'goo-local-results :results results))))

(defun goo-local-page-from-cache (entry-number)
  (with-open-file (entry-file (merge-pathnames (make-pathname :name (write-to-string entry-number) :type "html")
                                               *goo-local-cache*))
    (let ((entry-file-contents (make-array (file-length entry-file) :element-type 'character)))
      (read-sequence entry-file-contents entry-file :end (file-length entry-file))
      entry-file-contents)))

(defclass goo-local-results (search-results) ())

(defmethod search-results-number-of-pages ((search-results goo-local-results))
  (ceiling (length (search-results-sic search-results))
           (search-results-results-per-page search-results)))

(defmethod search-results-page-list ((search-results goo-local-results) page-number)
  (let* ((results (search-results-sic search-results))
         (results-per-page (search-results-results-per-page search-results))
         (start-value (* page-number results-per-page))
         (end-value (min (+ start-value results-per-page)
                         (- (length results) start-value))))
    (loop for selection-number from start-value below end-value
       collect (let* ((result (aref results selection-number))
                      (entry-number (aref result 1))
                      (entry-file-contents (goo-local-page-from-cache entry-number))
                      (definition (goo-definition-from-meaning-page entry-file-contents))
                      (peek (subseq definition 0 (min 40 (length definition)))))
                 (format nil "~A..." (cl-ppcre:regex-replace-all "(?m)\\n" peek ""))))))

(defmethod search-results-select-result ((search-results goo-local-results) page-number selection-number)

  (let* ((selection (aref (search-results-sic search-results)
                          (+ (* page-number (search-results-results-per-page search-results))
                             selection-number)))
         (entry-number (aref selection 1)))
    (goo-local-page-from-cache entry-number)))
