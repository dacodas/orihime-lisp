(in-package :orihime)

(defparameter *graph* (cl-graph:make-graph 'cl-graph:dot-graph))
(defparameter *parent* nil)

;; The original functions for grab-text and grab-child-words
((loop for row = (symbolize-sql-keys (dbi:fetch result))
         while row
         do
           (with-plist-properties (peek)
               row
             (format t "~S~%" row)
             (cl-graph:add-vertex *graph*
                                  text-id
                                  :dot-attributes `(:label ,peek :url nil))
             (grab-child-words text-id)))
      (let ((*parent* (or *parent* text-id)))
        (loop for row = (symbolize-sql-keys (dbi:fetch result))
           while row
           do
             (progn (format t "~S~%" row)
                    (with-plist-properties (word-id reading) row
                      (unless (cl-graph:find-vertex *graph* word-id nil)
                        (cl-graph:add-vertex *graph*
                                             word-id
                                             :dot-attributes `(:label ,reading :url nil))
                        (cl-graph:add-edge-between-vertexes *graph*
                                                            *parent*
                                                            word-id
                                                            :dot-attributes '(:label ""))
                        (format t "Okay, grabbing child words now for word id ~A...~%" word-id)
                        (let ((*parent* word-id))
                          (grab-child-words (grab-definition-text-id-from-word-id word-id) ))))))))

(defun/sql grab-all-texts-from-source (source-id)
  (let* ((query (dbi:prepare *connection*
                             (join-sql-strings
                              "SELECT id FROM texts"
                              "WHERE source_id = ?")))
         (result (dbi:execute query source-id)))

    (loop for row = (symbolize-sql-keys (dbi:fetch result))
       while row
       do (let ((*graph* (cl-graph:make-graph 'cl-graph:dot-graph)))

            (with-plist-properties (id) row
              (grab-text id)

              (let* ((basename (format nil "text-~A" id))
                     (base-pathname (make-pathname :directory `(:absolute "tmp" "orihime" "sources" ,(format nil "~d" source-id))
                                                   :name basename))
                     (dot-file (merge-pathnames (make-pathname :directory `(:relative "dot-files")
                                                               :type "dot")
                                                base-pathname))
                     (svg-file (merge-pathnames (make-pathname :directory `(:relative "svgs")
                                                               :type "svg")
                                                base-pathname))
                     (pdf-file (merge-pathnames (make-pathname :directory `(:relative "pdfs")
                                                               :type "pdf")
                                                base-pathname)))


                (loop for path in (list dot-file pdf-file svg-file)
                     do (ensure-directories-exist path))

                (with-open-file (output-file dot-file 
                                             :direction :output
                                             :if-exists :supersede
                                             :if-does-not-exist :create)
                  (cl-graph:graph->dot *graph* output-file))
                (let*-print ((args `("-Tpdf" "-o" ,(namestring pdf-file) ,(namestring dot-file)))
                             (return-code (sb-ext:run-program "/usr/bin/dot" args
                                                              :output *standard-output*
                                                              :error *standard-output*))))
                (let*-print ((args `("-Tsvg" "-o" ,(namestring svg-file) ,(namestring dot-file)))
                             (return-code (sb-ext:run-program "/usr/bin/dot" args
                                                              :output *standard-output*
                                                              :error *standard-output*))))))))))
