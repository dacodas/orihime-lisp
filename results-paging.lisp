(in-package :orihime)

(defclass search-results ()
  ((results
     :initarg :results
     :reader search-results-sic)
   (results-per-page
    :initarg :results-per-page
    :reader search-results-results-per-page
    :initform 10)))

(defgeneric search-results-number-of-pages (search-results))
(defgeneric search-results-page-list (search-results page-number))
(defgeneric search-results-select-result (search-results page-number selection-number))

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
                 (sb-int::simple-parse-error ()
                   (progn
                     (format t "That isn't a valid input~%")
                     (results-paging search-results page-number)))))))))
