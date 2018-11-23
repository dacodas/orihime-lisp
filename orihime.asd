;;;; orihime.asd

(asdf:defsystem orihime
  :description "Lispy word webs"
  :serial t
  :components ((:file "package")
               (:file "goo-results-paging")
	           (:file "goo-utils")
               (:file "goo-web")
               ;; (:file "graphs")
               (:file "cl-mongo-document-printing")
               (:file "serialization")
               (:file "class-definitions"))
  :depends-on (:log4cl :drakma :do-urlencode :plump :lquery :lparallel :cl-ppcre :swank :ironclad :cl-base64 :cl-mongo
                       :cl-containers :cl-graph :cl-mustache))
