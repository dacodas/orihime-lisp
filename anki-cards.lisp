(in-package :orihime)

(defparameter *output-folder* (make-pathname :directory '(:absolute "var" "lib" "anki" "lisp-output")))
(defparameter *cards-output* (merge-pathnames "cards/" *output-folder*))
(defparameter *model-output* (merge-pathnames "model/" *output-folder*))
(defparameter *python-exec* "/usr/bin/python3")
(defparameter *python-scripts* (merge-pathnames
                                "anki-scripts/"
                                (asdf:system-source-directory :orihime)))

(defparameter *anki-max-number-of-words* 10)

(defparameter *pretty-colors*
    '("red"
      "blue"
      "green"
      "purple"
      "orange"))

(defparameter *orihime-colors-style*
  (format nil "~{~A~}"
          (let ((count 0))
            (loop for i = (incf count)
               for color in *pretty-colors*
               collect
                 (with-output-to-string (*standard-output*)
                   (mustache-render "
strong.orihime-color-{{ color-number }} 
{
    color: {{ color }};
}
"
                                    `((:color-number . ,i)
                                      (:color . ,color))))))))

(defun import-anki-model ()

  (generate-anki-model)

  (let ((script-location (namestring (merge-pathnames "create_orihime_model.py" *python-scripts*))))
    (sb-ext:run-program *python-exec* (list script-location) 
                        :output *standard-output* 
                        :directory *python-scripts*)))

(defun import-anki-cards ()

  (generate-all-anki-cards)

  (let ((script-location (namestring (merge-pathnames "add_orihime_note.py" *python-scripts*))))
    (sb-ext:run-program *python-exec* (list script-location) 
                        :output *standard-output* 
                        :directory *python-scripts*)))

(defun/sql generate-all-anki-cards ()

  (uiop:delete-directory-tree *cards-output* :validate t)

  (let ((query (dbi:prepare *connection*
                            "select hex(hash) as hash from texts where source_id is not null;")))
    (process-multiple-sql-rows (dbi:execute query)
      (with-plist-properties (hash)
          row
          (generate-anki-card hash)))))

(defun generate-anki-card (text-hash)
  (let ((text-id (sql-text-id-from-hash text-hash)))
    (ensure-directories-exist *cards-output*)
    (with-open-file (output-file
                     (merge-pathnames (format nil "~A.json" text-hash) *cards-output*)
                     :direction :output
                     :if-exists :supersede
                     :if-does-not-exist :create)
      (format output-file "~A" 
              (let ((number-of-words 0))
                (labels ((text-processing (text-id)
                           (process-sql-row (grab-text text-id)
                             (with-plist-properties (peek contents)
                                 row
                               (child-words-processing contents text-id))))
                         (child-words-processing (text-contents text-id)
                           (let ((added-char-count 0)
                                 (previous-end 0)
                                 (row-count 0)
                                 (sql-response (grab-child-words text-id))
                                 (tagged-contents text-contents))
                             (loop for row = (symbolize-sql-keys (dbi:fetch sql-response))
                                for current-row = (incf row-count)
                                while row 
                                do
                                  (incf number-of-words)
                                  (with-plist-properties (beginning ending)
                                      row
                                    (let ((new-beginning (+ beginning added-char-count))
                                          (new-ending (+ ending added-char-count)))
                                      (if (< new-beginning previous-end)
                                          (format t "Not adding this word...~%")
                                          (progn
                                            (setf tagged-contents
                                                  (format nil "~A<strong class=\"orihime-color-~A\">~A</strong>~A"
                                                          (subseq tagged-contents 0 new-beginning)
                                                          (mod current-row (length *pretty-colors*))
                                                          (subseq tagged-contents new-beginning new-ending)
                                                          (subseq tagged-contents new-ending)))
                                            (incf added-char-count (+ 24 17))
                                            (setf previous-end (+ new-ending 17 24))))))
                                collect
                                  (with-plist-properties (reading word-id)
                                      row
                                    (let ((definition-text-id (grab-definition-text-id-from-word-id word-id)))
                                      `(:li :class "orihime-word" (:div :class "reading" ,reading)
                                            ,@(text-processing definition-text-id))))
                                into items
                                finally
                                  (return `((:div :class "definition" ,tagged-contents)
                                            ,(if items  
                                                 `(:div :class "child-words" (:ul ,@items))
                                                 "")))))))

                  (let* ((sexp `(cl-who:with-html-output (*standard-output* nil :indent t) 
                                  (cl-who:htm ,@(text-processing text-id))))
                         (html-output (eval sexp))
                         (present-fields (loop for i below *anki-max-number-of-words*
                                            collect (if (< i number-of-words) "1" ""))))
                    (with-output-to-string (*standard-output*)
                      (cl-json:encode-json-alist `((fields . ,(append (list html-output) present-fields))))))))))))

(defun render-template-to-string (template context)
  (with-output-to-string (*standard-output*)
    (mustache-render template context)))

(defun generate-anki-model ()
  (let* ((front-template (merge-pathnames "toggle-snippet-front.js.mustache" *templates-directory*))
         (back-template (merge-pathnames "toggle-snippet-back.js.mustache" *templates-directory*)))

    (ensure-directories-exist *model-output*)

    (loop for field-number below 10
       do 
         (let* ((front-output (merge-pathnames (format nil "~A/front.html" field-number) *model-output*))
                (back-output (merge-pathnames "back.html" front-output))
                (toggle-snippet-context `((:field-number . ,field-number)))
                (toggle-snippet-front (render-template-to-string front-template toggle-snippet-context))
                (toggle-snippet-back (render-template-to-string back-template toggle-snippet-context))
                (context `((:orihime-colors . ,*orihime-colors-style*)
                           (:field-number . ,field-number)
                           (:ANKI-field-number-start . ,(format nil "{{#~A}}" field-number))
                           (:ANKI-field-number-end . ,(format nil "{{/~A}}" field-number))
                           (:ANKI-field-number . ,(format nil "{{/~A}}" field-number))
                           (:ANKI-Text . "{{ Text }}"))))

           (ensure-directories-exist front-output)

           (loop for (output toggle-snippet) in (list (list front-output toggle-snippet-front)
                                                      (list back-output toggle-snippet-back))

              do (with-open-file (output-file output
                                              :direction :output
                                              :if-exists :supersede
                                              :if-does-not-exist :create)
                   (mustache-render
                    (merge-pathnames "html-template.html.mustache" *templates-directory*)
                    (append `((:toggle-snippet . ,toggle-snippet)) context)
                    output-file)))))))

