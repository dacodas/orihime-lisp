;;;; orihime.asd

(asdf:defsystem orihime
  :description "Lispy word webs"
  :serial t
  :components ((:file "package")
               (:file "class-definitions")
	           (:file "goo-utils")
               (:file "goo-web")
               (:file "goo-local")
               (:file "goo-results-paging")
               (:file "cl-mongo-document-printing")
               (:file "serialization")
               (:file "generate-anki-format")
               (:file "graphs"))
  :depends-on (:log4cl :drakma :do-urlencode :plump :lquery :lparallel :cl-ppcre :swank :ironclad :cl-base64 :cl-mongo
                       :cl-containers :cl-graph :cl-mustache))
