;;;; orihime.asd

(asdf:defsystem orihime
  :description "Lispy word webs"
  :serial t
  :components ((:file "package")
               (:file "orihime-results-paging")
	           (:file "orihime-parsing")
               (:file "class-definitions")
               (:file "cl-mongo-document-printing")
               (:file "orihime-serialization"))
  :depends-on (:log4cl :drakma :do-urlencode :plump :lquery :lparallel :cl-ppcre :swank :ironclad :cl-base64 :cl-mongo))
