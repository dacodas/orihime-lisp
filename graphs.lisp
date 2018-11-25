(in-package :orihime)

(defparameter *current-graph-words* nil)
(defparameter *output-root-directory* (make-pathname :directory "tmp"))
(defparameter *templates-directory* (merge-pathnames (make-pathname :directory '(:relative "static"))
                                     (asdf:system-source-directory :orihime)))
(defparameter *latex-template-file* (merge-pathnames (make-pathname :name "latex-template.tex" :type "mustache")
                                                     *templates-directory*))
(defparameter *text-id-contents-file-name-format* "~A-contents")
(defparameter *output-directory* nil)
(defparameter *word-names* nil)

;; (defparameter *dot-file-name* nil)
;; (defparameter *svg-file-name* nil)
;; (defparameter *pdf-file-name* nil)
(defun child-word-definition-text (child-word)
  (let ((definition-id (word-definition (gethash (word-reading child-word) *words*))))
    (gethash definition-id *texts*)) )

(defgeneric print-child-words (object stream))

(defmethod print-child-words ((text text) stream)
  (loop for word across (text-child-words text)
       do (print-child-words word stream)))

(defmethod print-child-words ((word child-word-in-context) stream)
  (format stream "Current word: ~A~%Children words: ~%" (word-reading word))
  (print-child-words (child-word-definition-text word) stream))

(defun remove-slashes (text)
  (cl-ppcre:regex-replace-all "/" text ""))

(defgeneric add-this-and-children-to-graph (object graph))

(defmethod add-this-and-children-to-graph ((text text) graph)
  (let* ((text-peek (get-text-peek text))
         (text-id (text-id text))
         (text-id-sans-slashes (remove-slashes text-id)))
    (cl-graph:add-vertex graph text-id-sans-slashes :dot-attributes `(:label ,text-peek :url ,text-id-sans-slashes))

    (let* ((tex-file-name (merge-pathnames (make-pathname :name (format nil *text-id-contents-file-name-format* text-id-sans-slashes) :type "tex") *output-directory*))
           (pdf-file-name (merge-pathnames (make-pathname :type "pdf") tex-file-name)))
      
      (with-open-file (output-file tex-file-name
                                   :direction :output
                                   :if-does-not-exist :create
                                   :if-exists :supersede)

        (mustache:render *latex-template-file* `((:content . ,(text-contents text))) output-file))
      (sb-ext:run-program "/usr/bin/xelatex" `(,(namestring tex-file-name)) :directory *output-directory*))

    (loop for word across (text-child-words text)
       do (progn
            (add-this-and-children-to-graph word graph)
            (cl-graph:add-edge-between-vertexes graph text-id-sans-slashes (word-reading word)
                                                :dot-attributes '(:label ""))))))

(defmethod add-this-and-children-to-graph ((word child-word-in-context) graph)
  (with-slots (word-reading) word
    (cl-graph:add-vertex graph word-reading :dot-attributes `(:label ,word-reading :url ,word-reading))

    (let* ((tex-file-name (merge-pathnames (make-pathname :name word-reading :type "tex") *output-directory*))
           (pdf-file-name (merge-pathnames (make-pathname :type "pdf") tex-file-name)))
      
      (with-open-file (output-file tex-file-name
                                   :direction :output
                                   :if-does-not-exist :create
                                   :if-exists :supersede)

        (mustache:render *latex-template-file* `((:content . ,(text-contents (child-word-definition-text word)))) output-file))
      (sb-ext:run-program "/usr/bin/xelatex" `(,(namestring tex-file-name)) :directory *output-directory*))

    (push word-reading *word-names*)
  
    (loop for child-word across (text-child-words (child-word-definition-text word))
       do (progn
            (add-this-and-children-to-graph child-word graph)
            (cl-graph:add-edge-between-vertexes graph word-reading (word-reading child-word)
                                                :dot-attributes '(:label ""))))))

(defun text-make-graph (text-id)
  (let* ((text-id-sans-slashes (remove-slashes text-id))
         (text (gethash text-id *texts*))
         (*output-directory* (make-pathname :directory (append (pathname-directory *output-root-directory*) (list text-id-sans-slashes))))
         (dot-file-name (merge-pathnames (make-pathname :name text-id-sans-slashes :type "dot") *output-directory*))
         (svg-file-name (merge-pathnames (make-pathname :type "svg") dot-file-name))
         (pdf-file-name (merge-pathnames (make-pathname :type "pdf") dot-file-name))
         (united-pdf-file-name (merge-pathnames (make-pathname :name (format nil "~A-united-output" text-id-sans-slashes)) pdf-file-name))
         (linked-pdf-file-name (merge-pathnames (make-pathname :name (format nil "~A-linked-output" text-id-sans-slashes)) pdf-file-name)))

    (ensure-directories-exist *output-directory*)

    (with-open-file (output-file dot-file-name
                                 :direction :output
                                 :if-exists :supersede
                                 :if-does-not-exist :create)
      (let ((graph (cl-graph:make-graph 'cl-graph:dot-graph)))
        (add-this-and-children-to-graph text graph)
        (cl-graph:graph->dot graph output-file)))

    (sb-ext:run-program "/usr/bin/dot" `(,(namestring dot-file-name) "-Tsvg" "-o" ,(namestring svg-file-name)))
    (sb-ext:run-program "/usr/bin/rsvg-convert" `("-f" "pdf" "-o" ,(namestring pdf-file-name) ,(namestring svg-file-name)))

    (let* ((pdf-files-in-order (append
                                (list pdf-file-name
                                      (merge-pathnames (make-pathname :name (format nil *text-id-contents-file-name-format* text-id-sans-slashes) :type "pdf")
                                                       *output-directory*))
                                (loop for word in *word-names* collect
                                     (merge-pathnames (make-pathname :name word :type "pdf")
                                                      *output-directory*))))
           (pdfunite-files (append pdf-files-in-order (list united-pdf-file-name))))
      (sb-ext:run-program "/usr/bin/pdfunite" (mapcar #'namestring pdfunite-files)
                          :output "/tmp/output"
                          :if-output-exists :supersede)

      (let ((arguments (append `(,(namestring united-pdf-file-name) ,text-id) *word-names* `(,(namestring linked-pdf-file-name)))))
        (format t "~A" arguments)
        (sb-ext:run-program  "/home/dacoda/pdf-testing/testing" arguments
                             :environment '("LD_LIBRARY_PATH=/home/dacoda/poppler/build/")
                             :output "/tmp/output"
                             :if-output-exists :supersede)))))




