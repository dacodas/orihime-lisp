(in-package :orihime)

(defclass search-results ()
  ((results
     :initarg :results
     :reader search-results-sic)
   (results-per-page
    :initarg :results-per-page
    :reader search-results-results-per-page
    :initform 10)))

(defclass goo-local-results (search-results) ())
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

(defgeneric search-results-number-of-pages (search-results))
(defgeneric search-results-page-list (search-results page-number))
(defgeneric search-results-select-result (search-results page-number selection-number))

(defmethod search-results-number-of-pages ((search-results goo-web-results))
    (length (search-results-sic search-results)))

(defmethod search-results-page-list ((search-results goo-web-results) page-number)
  (let* ((body (first (multiple-value-list (lparallel:force (aref result-futures page-number)))))
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
    (grab-goo-relative-page (goo-web-results-get-anchor page-number selection-number)))

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
                      (definition (definition-from-goo-meaning-page entry-file-contents))
                      (peek (subseq definition 0 (min 20 (length definition)))))
                 (format nil "~A..." (cl-ppcre:regex-replace-all "(?m)\\n" peek ""))))))

(defmethod search-results-select-result ((search-results goo-local-results) page-number selection-number)

  (let* ((selection (aref (search-results-sic search-results)
                          (+ (* page-number (search-results-results-per-page search-results))
                             selection-number)))
         (entry-number (aref selection 1)))
    (goo-local-page-from-cache entry-number)))

(defun goo-local-page-from-cache (entry-number)
  (with-open-file (entry-file (merge-pathnames (make-pathname :name (write-to-string entry-number) :type "html")
                                               *goo-local-cache*))
    (let ((entry-file-contents (make-array (file-length entry-file) :element-type 'character)))
      (read-sequence entry-file-contents entry-file :end (file-length entry-file))
      entry-file-contents)))

(defun attempt-page (search-results new-page-number old-page-number)
  (if (and (<= new-page-number (search-results-number-of-pages search-results))
           (>= new-page-number 0))
      (results-paging search-results new-page-number)
      (progn
        (format t "That page number isn't valid...~%")
        (results-paging search-results old-page-number))))

(defun results-paging (search-results &optional (page-number 0))
  (let ((current-page-results (search-results-page-list search-results page-number)))
    (format t "Showing results for page ~A~%~%" page-number)

    (loop for result in current-page-results
       for result-number below (length current-page-results)
       do (format t "~A - ~A~%" result-number result))
    
    (format t "~%Enter a number to access the definition, 'n' to go to the next page, and 'p' to go the previous page:~%")

    (let ((user-input (read-line))
          (parsed-integer (handler-case (parse-integer user-input)
                            (error (c) nil))))
      (cond ((equal user-input "n") (attempt-page search-results (1+ page-number) page-number))
            ((equal user-input "p") (attempt-page search-results (1- page-number) page-number))
            (t (handler-case (let ((parsed-integer (parse-integer user-input)))
                               (if (and (>= parsed-integer 0) (< parsed-integer (search-results-number-of-pages search-results)))
                                   (progn
                                     (format t "Going to selection ~A~%" parsed-integer)
                                     (search-results-select-result search-results page-number parsed-integer))
                                   (progn
                                     (format t "That number is too large or too small~%")
                                     (results-paging search-results page-number)))) 
                 (error (simple-parse-error)
                   (progn
                     (format t "That isn't a valid input~%")
                     (results-paging search-results page-number)))))))))
