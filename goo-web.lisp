(defparameter *max-number-of-result-pages-to-query* 50)
(defparameter *user-agent* "Opera/9.80 (Windows NT 6.0) Presto/2.12.388 Version/12.14")
(defparameter *cookie-jar* (make-instance 'drakma:cookie-jar))
(setf lparallel:*kernel* (lparallel:make-kernel 8))

(defun fix-puri-uri (uri)
  (let ((uri-stream (make-string-output-stream)))

    (puri:render-uri uri uri-stream)

    (let* ((uri-latin-1-string (get-output-stream-string uri-stream))
           (uri-octets (babel:string-to-octets uri-latin-1-string :encoding :latin-1))
           (uri-unicode-string (babel:octets-to-string uri-octets)))

      uri-unicode-string)))

(lquery:define-lquery-function text-without-comments (node)
  (with-output-to-string (stream)
    (labels ((r (node)
               (loop for child across (plump::children node)
		          do (cond ((and (typep child 'plump::textual-node) (not (typep child 'plump::comment))) (write-string (plump::text child) stream)) 
			               ((typep child 'plump::nesting-node) (r child))))))
      (r node))))

(export 'lquery-funcs::text-without-comments :lquery-funcs)

(defun grab-goo-relative-page (relative-path)
  (drakma:http-request (concatenate 'string "https://dictionary.goo.ne.jp" relative-path)
                       :user-agent *user-agent* 
                       :cookie-jar *cookie-jar*))

(defun grab-goo-response (word)
  (drakma:http-request (concatenate 'string "https://dictionary.goo.ne.jp/srch/jn/" (do-urlencode:urlencode word) "/m0u/")
                       :user-agent *user-agent* 
                       :cookie-jar *cookie-jar*))


(defun get-number-of-results-pages (results-page-dom)
  (let* ((list-items (lquery:$ results-page-dom "div.nav-paging ol.nav-paging-in li"))
         (number-of-list-items (length list-items)))
    (cond ((eq 0 number-of-list-items) 1)
          (t (let* ((last-numbered-list-item (aref list-items (- number-of-list-items 2)))
                    (last-number (parse-integer (lquery:$1 last-numbered-list-item (lquery-funcs:text)))))
               last-number)))))


(defun get-results-page-url (first-results-page-url page-number)
  (let ((uri-stream (make-string-output-stream)))
    (puri:render-uri first-results-page-url uri-stream)
    (let ((uri-string (get-output-stream-string uri-stream)))
      (cl-ppcre:regex-replace "m0u" uri-string (format nil "m0p~au" page-number)))))

(defun get-results-page-list-items (body-dom)
  (lquery:$ body-dom "div.search ul.list-search-a li"))

(defun fill-upper-pages (result-futures search-result-url)
  (loop for page-number from 2 upto (length result-futures)
     do (let* ((page-url (get-results-page-url search-result-url page-number)))
          (setf (aref result-futures (1- page-number)) 
                (lparallel:future (drakma:http-request page-url
                                                       :user-agent *user-agent*
                                                       :cookie-jar *cookie-jar*))))))

(defun get-results-pages-futures (response)
  (multiple-value-bind (body status headers uri) (values-list response)
    (let* ((dom (lquery:$ (lquery:initialize body)))
           (number-of-results-pages (get-number-of-results-pages dom))
           (result-futures (make-array (min number-of-results-pages *max-number-of-result-pages-to-query*) :initial-element nil)))

      (setf (aref result-futures 0) (lparallel:future (values-list response)))

      (if (> number-of-results-pages 1)
          (fill-upper-pages result-futures uri))

      result-futures)))

;; This should return multiple values
;; 1. A vector of futures representing the pages of the response
;; 2. A keyword specifying the type of response we got
;;
;; NOTE: The response passed in here is FOR SOME REASON a list instead
;; of multiple values... maybe that's the only way to do it
(defun parse-response (response)
  (multiple-value-bind (body status headers uri) (values-list response)
    (let ((proper-uri (fix-puri-uri uri)))
      (cond
        ((search "meaning" proper-uri)
         (let ((result-futures `#(,(lparallel:future (values-list response)))))
           (values-list `(,result-futures :meaning))))
        ((search "srch" proper-uri)
         (values-list `(,(get-results-pages-futures response) :results-page)))
        ((t (error "The returned URI from the response is unexpected: ~a" proper-uri)))))))


(define-condition entry-number-parse-error (error)
  ((uri :initarg :uri :reader uri))
  (:report
   (lambda (condition stream)
     (format stream
             "Could not parse an entry number from the URI ~A"
             (uri condition)))))

(defun entry-number-from-response (response)
  (multiple-value-bind (x y z uri)
      (values-list response)

    (let ((potential-matching-regexes (list ".*jn/\([0-9]+\)/meaning.*"
                                            ".*word/.*/#jn-\([0-9]+\)")))

      (let ((regex-match
             (loop for regex in potential-matching-regexes
                for (full-match groups) = (multiple-value-list (cl-ppcre:scan-to-strings regex (fix-puri-uri uri)))
                when (not (null full-match))
                do (return (aref groups 0)))))

        (if (null regex-match)
            (error 'entry-number-parse-error :uri (fix-puri-uri uri))
            (parse-integer regex-match))))))
