(in-package :orihime)

(defclass goo-web-results (search-results)
  ;; This is a 2D vector
  ;; - The first index is the page number
  ;; - The second index is the selection number
  ;; - The value is the target of the anchor
  ((page-anchors :accessor goo-web-results-page-anchors)))

;; Make sure to initialize vector for the page-anchors
(defmethod initialize-instance :after ((search-results goo-web-results) &key)

  (if (not (equal 10 (search-results-results-per-page search-results)))
      (error "A goo-web-results instance needs to have 10 results per page"))

  (let ((page-futures (search-results-sic search-results)))
    (setf (goo-web-results-page-anchors search-results)
          (make-array (length page-futures)))))

(defun goo-web-results-get-anchor (goo-web-results page-number selection-number)
    (aref 
     (aref
      (goo-web-results-page-anchors goo-web-results)
      page-number)
     selection-number))

(defmethod search-results-number-of-pages ((search-results goo-web-results))
    (length (search-results-sic search-results)))

(defmethod search-results-page-list ((search-results goo-web-results) page-number)
  (let* ((body (first (multiple-value-list (lparallel:force (aref (search-results-sic search-results) page-number)))))
         (body-dom (lquery:$ (lquery:initialize body)))
         (list-items (get-results-page-list-items body-dom))
         (list-contents (lquery:$
                          list-items
                          (combine (lquery:$ "dt.title" (lquery-funcs:text))
                                   (lquery:$ "dd.mean" (lquery-funcs:text))
                                   (lquery:$ "a" (lquery-funcs:attr :href)))
                          (lquery-funcs:map-apply (lambda (&rest args)
                                                    (mapcar (lambda (vector) (aref vector 0))
                                                            args))))))
    (setf (aref (goo-web-results-page-anchors search-results) page-number)
          (make-array (length list-contents)))

    (loop for (title meaning-text anchor) across list-contents
       for result-number below (length list-contents)
       do (setf
           (aref 
            (aref (goo-web-results-page-anchors search-results) page-number)
            result-number)
           anchor)
       collect 
         (let ((text-peek (subseq meaning-text 0 (min (length meaning-text) 40))))
           (format nil "~A：~A..."
                   title
                   text-peek)))))

(defmethod search-results-select-result ((search-results goo-web-results) page-number selection-number)
    (grab-goo-relative-page (goo-web-results-get-anchor search-results page-number selection-number)))

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

(defun grab-goo-relative-page (relative-path)
  (drakma:http-request (concatenate 'string "https://dictionary.goo.ne.jp" relative-path)
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
  (multiple-value-bind (body status headers uri)
      (values-list response)
    (let* ((dom (lquery:$ (lquery:initialize body)))
           (number-of-results-pages (get-number-of-results-pages dom))
           (result-futures (make-array (min number-of-results-pages *max-number-of-result-pages-to-query*) :initial-element nil)))

      (setf (aref result-futures 0) (lparallel:future (values-list response)))

      (if (> number-of-results-pages 1)
          (fill-upper-pages result-futures uri))

      result-futures)))

(defun goo-web-search (reading)
  (multiple-value-bind (body status headers uri)
      (drakma:http-request (concatenate 'string "https://dictionary.goo.ne.jp/srch/jn/" (do-urlencode:urlencode reading) "/m0u/")
                           :user-agent *user-agent* 
                           :cookie-jar *cookie-jar*)
    (let ((proper-uri (fix-puri-uri uri)))
      (cond
        ((search "meaning" proper-uri)
         body)
        ((search "srch" proper-uri)
         (results-paging (make-instance 'goo-web-results :results (get-results-pages-futures (list body status headers uri)))))
        ((t (error "The returned URI from the response is unexpected: ~a" proper-uri)))))))
