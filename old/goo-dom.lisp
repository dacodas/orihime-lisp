(defun call-dot (dot-pathname pdf-pathname)
  (sb-ext:run-program "/usr/sbin/dot" (list "-Tpdf" (namestring dot-pathname) "-o" (namestring pdf-pathname))))

(defun contents->pdf (contents pdf-pathname &key title)
  (let ((dot-pathname (make-pathname :defaults pdf-pathname :type "dot")))
    (with-open-file (output-file-stream dot-pathname
					:direction :output
					:if-exists :supersede
					:if-does-not-exist :create)
      (format output-file-stream (contents->graphviz contents :title title)))
    (call-dot dot-pathname pdf-pathname)
     pdf-pathname))

(defun pdfunite (output-file input-files)
  (sb-ext:run-program "/usr/sbin/pdfunite" 
		      (concatenate 'list (mapcar #'namestring input-files) (list (namestring output-file)))))

(defun contents->graphviz (contents &key title)

  (let ((elements-node-map (make-hash-table :test #'eq))
        (node-count 0)
        (output-stream (make-string-output-stream)))

    (defun make-nodes (element)
      (if (eq nil element)
          nil
          (progn
            (log:debug "This element is ~A and has class ~A and will be assigned node ~A" element (class-of element) node-count)

            (setf (gethash element elements-node-map) node-count)

            (cond ((plump:text-node-p element) (progn (format output-stream "~A [label=\"~A\"];~%" node-count "text") (incf node-count)))
                  ((typep element 'plump:comment )(progn (format output-stream "~A [label=\"~A\"];~%" node-count "comment") (incf node-count)))
                  (t (progn (format output-stream "~A [label=\"~A\"];~%" node-count (element-tag element))
                            (incf node-count)
                            (map 'nil #'make-nodes (plump:children element)))))))

      )

    (defun connect-nodes (element)

      (if (eq nil element)
          nil
          (progn

            (let ((node (gethash element elements-node-map)))

              (log:debug "Working with element ~A of class ~A and node value ~A" element (class-of element) node)

              (cond ((plump:text-node-p element) (log:debug "This is a text node (and thus has no children)..."))
                    ((typep element 'plump:comment (log:debug "This is a comment (and has no children)")))
                    ((equalp (vector) (plump:children element)) (log:debug "This is an element, but it has no children..."))
                    (t (progn
                         (let ((children-nodes (plump:children element)))
                           (log:debug "This has children... it is node ~A and is attached to ~{~A~^, ~}" node (map 'list (lambda (child) (gethash child elements-node-map)) children-nodes))
                           (map nil (lambda (child) (format output-stream "~A -> ~A;~%" node (gethash child elements-node-map))) children-nodes)
                           (map 'vector #'connect-nodes (plump:children element)))))))))
      t)

    (format output-stream "~A~%" "digraph BST {")
    (if title (format output-stream "graph [label=\"~A\", labelloc=top];~%" title))
    (format output-stream "~A~%" "node [fontname=\"Arial\"];")
    (make-nodes contents)
    (connect-nodes contents)
    (format output-stream "~A~%" "}")
    (get-output-stream-string output-stream)))

;; How can I combine the below two macros into a single macro? 
(defmacro test-with-randoms-and-collect (number-of-times &body body)
  `(loop for i below ,number-of-times collect
       (let* ((random-number (random 299999 (make-random-state t)))
            (url (concatenate 'string "https://dictionary.goo.ne.jp/jn/" (write-to-string random-number) "/meaning/m0u/"))
            (page (plump:parse (drakma:http-request url))))
       (log:debug "Testing with ~a" (write-to-string random-number))
       ,@body
       ))
  )

(defmacro test-with-randoms (&body body)
  `(dotimes (i 10)
     (let* ((random-number (random 299999 (make-random-state t)))
            (url (concatenate 'string "https://dictionary.goo.ne.jp/jn/" (write-to-string random-number) "/meaning/m0u/"))
            (page (plump:parse (drakma:http-request url))))
       (log:debug "Testing with ~a" (write-to-string random-number))
       ,@body
       )))

(defvar *test-pages*
  (test-with-randoms-and-collect 20
    page))


;; (time (pdfunite (make-pathname :directory '(:absolute "tmp") :name "composite" :type "pdf")
;;                 (mapcar (lambda (page count)
;;                           (let ((file-pathname (make-pathname
;;                                                 :directory '(:absolute "tmp")
;;                                                 :name (format nil "skeleton.~A" count)
;;                                                 :type "pdf")))
;;                             (contents->pdf (entry-contents (page-entry page)) file-pathname
;;                                            :title (format nil "~A\\n~A" (format nil "~{~a~^ → ~}" (navbar-to-list (page-navbar page))) (title-prettify (entry-title (page-entry page))))))) 
;;                         *test-pages*
;;                         (loop for i from 1 upto (length *test-pages*) collect i))))




(defun get-skeleton (element)
  (let ((children (lquery-funcs:children element)))
    (if (equalp children (vector))
        (progn
	  (log:debug "No more children!")
	   element)
        (progn
	  (log:debug "We're going another level!")
	  (log:debug children)
	  (cons element
		(map 'list
		     (lambda (child) (get-skeleton child))
		     children))))))

(defun skeleton-just-element-tags (skeleton)
  (cond ((eq nil skeleton) nil)
	((listp skeleton) (cons (skeleton-just-element-tags (car skeleton)) (skeleton-just-element-tags (cdr skeleton)))) 
	(t (element-tag skeleton))))

(defun skeleton-has-only-text-elements (skeleton)
  (cond ((eq nil skeleton) t)
        ((listp skeleton) (and (skeleton-has-only-text-elements (car skeleton))
                               (skeleton-has-only-text-elements (cdr skeleton))))
        (t (if (position (element-tag skeleton) '(|p| |div| |a| |strong| |span| |div|)) t))))

(defun skeleton-replace-with-text (skeleton)
  (cond ((listp skeleton) (cons (car skeleton) (mapcar #'skeleton-replace-with-text (cdr skeleton))))
        ((skeleton-has-only-text-elements skeleton) (lquery-funcs:text skeleton))
        (t skeleton)))
(defun element-tag (element)
  (intern (nth 1 (split-sequence:SPLIT-SEQUENCE #\Space (format nil "~A" element)))))

;; Testing lexing...
;; (defun munch-mecab-output (line-of-mecab-output)
;;   (let* ((first-split (split-sequence:SPLIT-SEQUENCE #\Tab line-of-mecab-output))
;;          (name (car first-split))
;;          (fields (split-sequence:SPLIT-SEQUENCE #\, (cadr first-split))))
;;     (concatenate 'list (list name) fields)))

;; (defun lex-text (text-to-lex)
;;   (let* ((input-stream (make-string-input-stream text-to-lex))
;;          (process (sb-ext:run-program "/usr/bin/mecab" '()
;;                                       :input input-stream
;;                                       :output :stream
;;                                       :wait nil)))
;;       (loop for line = (read-line (process-output process) nil :eof)
;;                                   until (eq line :eof)
;;          collect (munch-mecab-output line))))

;; (loop for token in
;;       (lex-text "［名］(スル)銃砲を発射するとき、火薬ガスの圧力が、弾丸を腔内から発射させると同時に、銃砲そのものを後方へ押しやること。")
;;       when (not (member (cadr token) '("記号" "助詞") :test #'equal))
;;       collect token
;;       )
