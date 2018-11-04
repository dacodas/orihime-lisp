(in-package :goo)

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

(defvar *words* (make-hash-table :test #'equal))
(defvar *texts* (make-hash-table :test #'equalp))
(defvar *definitions* (make-array 10 :adjustable t :fill-pointer 0))

(defparameter *max-number-of-result-pages-to-query* 50)
(defparameter *user-agent* "Opera/9.80 (Windows NT 6.0) Presto/2.12.388 Version/12.14")
(defparameter *cookie-jar* (make-instance 'drakma:cookie-jar))
(setf lparallel:*kernel* (lparallel:make-kernel 8))
(log:config :debug)

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


(defun trim-and-replace-big-breaks (text)
  (let* ((big-breaks-removed (cl-ppcre:regex-replace-all "(?m)\\n{3,}" text (format nil "~%~%")))
         (trimmed (string-trim '(#\Space #\Newline) big-breaks-removed)))
    trimmed))

(defun definition-from-goo-meaning-page (body)
  (let* ((title (lquery:$1 (initialize body)
                           "div.basic_title h1" (text-without-comments)))
         (definition-ugly (lquery:$1 (initialize body)
                                     "div.meaning_area div.contents" (text-without-comments)))
         (definition-pretty (trim-and-replace-big-breaks definition-ugly)))
    (format nil "~A~%~%~A" title definition-pretty)))

;; The goo-word-to-study should already have a proper goo-search
;; member
(defun determine-goo-word-meaning-page (goo-word-to-study)
  (let* ((this-goo-search (goo-search goo-word-to-study))
         (result-type (goo-result-type this-goo-search)))
    (alexandria:switch (result-type :test #'eq)
      (:meaning (lparallel:force (aref (goo-search-results this-goo-search) 0)) )
      (:results-page (results-paging (goo-search-results this-goo-search) 0))
      (t (error "It is not possible to get a word meaning page from result with type ~A" result-type)))))

(defun entry-number-from-response (response)
  (multiple-value-bind (x y z uri)
      (values-list response)

    (multiple-value-bind (full-match groups)
        (cl-ppcre:scan-to-strings ".*jn/\([0-9]+\)/meaning.*" (fix-puri-uri uri))

      (parse-integer (aref groups 0)))))

(defun make-definition (contents)
  (let ((definition (make-text contents :type :definition)))
    (vector-push-extend definition *definitions* 10)
    definition))

(defun make-text (contents &key (type :unspecified))
  (let* ((hash (cl-base64:usb8-array-to-base64-string (ironclad:digest-sequence :sha256 (sb-ext:string-to-octets contents))))
         (this-text (make-instance 'text :contents contents :id hash :type type)))
    (setf (gethash hash *texts*) this-text)))

(defun fill-goo-word-to-study (goo-word-to-study)
  (let* ((reading (word-reading goo-word-to-study))
         (response (multiple-value-list (grab-goo-response reading))))

    (multiple-value-bind (futures-list result-type)
        (parse-response response)
      (let ((this-goo-search (make-instance 'goo-search
                                             :search-term reading
                                             :results futures-list
                                             :result-type result-type)))
        (setf (slot-value goo-word-to-study 'goo-search) this-goo-search))))

  (let* ((entry-response (multiple-value-list (determine-goo-word-meaning-page goo-word-to-study)))
         (this-goo-entry (make-instance 'goo-entry
                                        :entry entry-response
                                        :entry-number (entry-number-from-response entry-response))))

    (setf (slot-value goo-word-to-study 'goo-entry) this-goo-entry)

    (let ((this-text (make-definition (definition-from-goo-meaning-page (first entry-response)))))
      (setf (word-definition goo-word-to-study) this-text))))

(defun make-goo-word-to-study (reading)
  (let ((new-word (make-instance 'goo-word-to-study :reading reading)))
    (setf (gethash reading *words*) new-word)
    new-word))

(defun grab-or-make-word (reading)
  (let ((hash-result (gethash reading *words*)))
    (if hash-result 
        hash-result
        (make-goo-word-to-study reading))))

(defun get-text-from-id (id)
  (text-contents (gethash id *texts*)))

(defun get-word-definition-id (reading)
  (let* ((word (grab-or-make-word reading))
         (definition-object (word-definition word)))
    (if (not definition-object)
        (progn
          (fill-goo-word-to-study word)
          (setf definition-object (word-definition word))))

    (text-id definition-object)))
