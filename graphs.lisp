(in-package :goo)

(ql:quickload '(cl-containers cl-graph))

(defparameter *current-graph-words* nil)
(defparameter *output-root-directory* (make-pathname :directory "tmp"))
(defparameter *latex-template-file* (merge-pathnames (make-pathname :name "latex-template.tex" :type "mustache")
                                                     (asdf:system-source-directory :goo)))
(defparameter *text-id-contents-file-name-format* "~A-contents")
(defparameter *output-directory* nil)
(defparameter *word-names* nil)

;; (defparameter *dot-file-name* nil)
;; (defparameter *svg-file-name* nil)
;; (defparameter *pdf-file-name* nil)

(defgeneric print-child-words (object stream))

(defmethod print-child-words ((text text) stream)
  (loop for word across (text-child-words text)
       do (print-child-words word stream)))

(defmethod print-child-words ((word child-word-in-context) stream)
  (format stream "Current word: ~A~%Children words: ~%" (word-reading word))
  (print-child-words (child-word-definition-text word) stream))

(defgeneric add-this-and-children-to-graph (object graph))

(defmethod add-this-and-children-to-graph ((text text) graph)
  (let ((text-peek (get-text-peek text))
        (text-id (text-id text)))
    (cl-graph:add-vertex graph text-id :dot-attributes `(:label ,text-peek :url ,text-id))

    (let* ((tex-file-name (merge-pathnames (make-pathname :name (format nil *text-id-contents-file-name-format* text-id) :type "tex") *output-directory*))
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
            (cl-graph:add-edge-between-vertexes graph text-id (word-reading word)
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

(let* ((text-id "O3hcBbp9LFL1gLBvc3G5b3gNU1lORuuAPf08qpADjhM=")
       (text (gethash text-id *texts*))
       (*output-directory* (make-pathname :directory (append (pathname-directory *output-root-directory*) (list text-id))))
       (dot-file-name (merge-pathnames (make-pathname :name text-id :type "dot") *output-directory*))
       (svg-file-name (merge-pathnames (make-pathname :type "svg") dot-file-name))
       (pdf-file-name (merge-pathnames (make-pathname :type "pdf") dot-file-name))
       (final-pdf-file-name (merge-pathnames (make-pathname :name "output") pdf-file-name)))

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
                                    (merge-pathnames (make-pathname :name (format nil *text-id-contents-file-name-format* text-id) :type "pdf")
                                                     *output-directory*))
                              (loop for word in *word-names* collect
                                   (merge-pathnames (make-pathname :name word :type "pdf")
                                                    *output-directory*))))
         (pdfunite-files (append pdf-files-in-order (list final-pdf-file-name))))
    (sb-ext:run-program "/usr/bin/pdfunite" (mapcar #'namestring pdfunite-files))))
