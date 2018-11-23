(in-package :orihime)

(defvar *words* (make-hash-table :test #'equal))
(defvar *texts* (make-hash-table :test #'equalp))
(defvar *definitions* (make-array 10 :adjustable t :fill-pointer 0))

(log:config :debug)

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
                                        :entry (make-array (length entry-response) :initial-contents entry-response)
                                        :entry-number (entry-number-from-response entry-response))))

    (setf (slot-value goo-word-to-study 'goo-entry) this-goo-entry)

    (let ((this-text (make-definition (definition-from-goo-meaning-page (first entry-response)))))
      (setf (word-definition goo-word-to-study) (text-id this-text)))))

(defun make-goo-word-to-study (reading)
  (let ((new-word (make-instance 'goo-word-to-study :word-reading reading)))
    (setf (gethash reading *words*) new-word)
    new-word))


