(in-package :goo)

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
