;; (in-package :goo-parsing)

(ql:quickload '(lparallel cl-ppcre))

;; Exact hit 怠る
;; Only one results page 怠り
;; Many results　怠
;; Many results　い
(defparameter *words-to-initialize* (list "怠る" "怠り" "怠" "い"))
(defparameter *max-number-of-result-pages-to-query* 50)
(defparameter *user-agent* "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/44.0.2403.157 Safari/537.36")
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


(lquery:define-lquery-function text-without-comments (node)
  (with-output-to-string (stream)
    (labels ((r (node)
               (loop for child across (plump::children node)
		          do (cond ((and (typep child 'plump::textual-node) (not (typep child 'plump::comment))) (write-string (plump::text child) stream)) 
			               ((typep child 'plump::nesting-node) (r child))))))
      (r node))))


(defun fix-puri-uri (uri)
  (let ((uri-stream (make-string-output-stream)))

    (puri:render-uri uri uri-stream)

    (let* ((uri-latin-1-string (get-output-stream-string uri-stream))
           (uri-octets (babel:string-to-octets uri-latin-1-string :encoding :latin-1))
           (uri-unicode-string (babel:octets-to-string uri-octets)))

      uri-unicode-string)))

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

(defun fill-result-futures (result-futures search-result-url)
  (loop for page-number from 2 upto (length result-futures)
     do (let* ((page-url (get-results-page-url search-result-url page-number)))
          (setf (aref result-futures (1- page-number)) 
                (lparallel:future (drakma:http-request page-url
                                                       :user-agent *user-agent*
                                                       :cookie-jar *cookie-jar*))))))

(defun get-results-page-list-items (body-dom)
  (lquery:$ body-dom "div.search ul.list-search-a li"))

(defun handle-results-page (response)
  (multiple-value-bind (body status headers uri) (values-list response)
    (let* ((dom (lquery:$ (lquery:initialize body)))
           (number-of-results-pages (get-number-of-results-pages dom))
           (result-futures (make-array (min number-of-results-pages *max-number-of-result-pages-to-query*) :initial-element nil)))

      (setf (aref result-futures 0) (lparallel:future (values-list response)))

      (if (> number-of-results-pages 1)
          (fill-result-futures result-futures uri))

      (values-list '(,result-futures :results-page)))))

(defun results-paging (result-futures page-number)
  (multiple-value-bind (body status headers uri)
      (lparallel:force (aref result-futures page-number))
    (let* ((body-dom (lquery:$ (lquery:initialize body)))
           (list-items (get-results-page-list-items body-dom))
           (list-titles-and-mean-texts (lquery:$
                                         list-items
                                         (combine (lquery:$ "dt.title" (lquery-funcs:text))
                                                  (lquery:$ "dd.mean" (lquery-funcs:text))
                                                  (lquery:$ "a" (lquery-funcs:attr :href)))
                                         (lquery-funcs:map-apply (lambda (&rest args)
                                                                   (mapcar (lambda (vector) (aref vector 0))
                                                                           args))))))

      #+nil(progn (log:info "Here is the response ~A" status)
             (log:info "Here is the body ~A" body)
             (log:info "Here are the list items ~A" list-items)
             (log:info "Here are the titles and mean texts ~s" list-titles-and-mean-texts))

      (format t "Showing results for page ~A~%~%" page-number)

      (loop for (title mean-text anchor) across list-titles-and-mean-texts
         for result-number below (length list-titles-and-mean-texts)
         do
           (format t "~A - ~A：~A...~%" result-number
                   title
                   (subseq mean-text 0 (min (length mean-text) 40))
                   anchor))

      (format t "~%Enter a number to access the definition, 'n' to go to the next page, and 'p' to go the previous page:~%")
      (let ((user-input (read-line))
            (parsed-integer (handler-case (parse-integer user-input)
                              (error (c) nil))))
        (cond ((equal user-input "n") (results-paging result-futures (1+ page-number)))
              ((equal user-input "p") (results-paging result-futures (1- page-number)))
              (t (handler-case (let ((parsed-integer (parse-integer user-input)))
                                 (if (and (>= parsed-integer 0) (< parsed-integer (length list-titles-and-mean-texts)))
                                     (progn
                                       (format t "Going to selection ~A~%" parsed-integer)
                                       (grab-goo-relative-page (third (aref list-titles-and-mean-texts parsed-integer))))
                                     (progn
                                       (format t "That number is too large or too small~%")
                                       (results-paging result-futures page-number)))) 
                   (error (simple-parse-error)
                     (progn
                       (format t "That isn't a valid input~%")
                       (results-paging result-futures page-number))))))))))

;; This should return multiple values
;; 1. A list of futures representing the pages of the response
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
         (handle-results-page response))
        ((t (error "The returned URI from the response is unexpected: ~a" proper-uri)))))))

(defun make-goo-word-to-study (reading)
  (make-instance 'goo-word-to-study :reading reading))

(defun trim-and-replace-big-breaks (text)
  (let* ((big-breaks-removed (cl-ppcre:regex-replace-all "(?m)\\n{3,}" text (format nil "~%~%")))
         (trimmed (string-trim '(#\Space #\Newline) big-breaks-removed)))
    trimmed))

(defun definition-from-goo-meaning-page (body)
  (let* ((definition-ugly (lquery:$1 (lquery:initialize body)
                                     "div.meaning_area"
                                     (lquery::text-without-comments)))
         (definition-pretty (trim-and-replace-big-breaks definition-ugly)))
    definition-pretty))

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

    (setf (word-definition goo-word-to-study)
          (definition-from-goo-meaning-page (first entry-response)))))

(defun initialized-words-temporary-format (word)
  (format nil "~A-response" word))

(defun initialized-words-global-format (word)
  (format nil "*~A-response*" word))

(defun initialized-words-temporary-variable (word)
  (intern (string-upcase (initialized-words-temporary-format word))))

(defun initialized-words-global-variable (word)
  (intern (string-upcase (initialized-words-global-format word))))

(defmacro uninitialize-words (words)
  (append '(progn)
          (loop for word in (symbol-value words)
             collect `(makunbound (quote ,(initialized-words-global-variable word))))))

(defmacro initialize-words (words)
  (loop for word in (symbol-value words)
     collecting `(,(initialized-words-temporary-variable word)
                (lparallel:future (multiple-value-list (grab-goo-response ,word))))
     into let-clauses
     collecting `(defvar ,(initialized-words-global-variable word)
                   (lparallel:force ,(initialized-words-temporary-variable word)))
     into defvar-clauses
     finally (return
               (append `(let ,let-clauses) defvar-clauses))))

(initialize-words *words-to-initialize*)

(defparameter *test-pages*
  (loop for word in *words-to-initialize*
     collect (symbol-value (initialized-words-global-variable word))))
