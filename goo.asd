;;;; goo.asd

(asdf:defsystem goo
  :description "Lispy interface to dictionary.goo.ne.jp"
  :serial t
  :components ((:file "package")
               (:file "goo-results-paging")
	           (:file "goo-parsing")
               (:file "class-definitions")
               (:file "cl-mongo-document-printing")
               (:file "goo-serialization"))
  :depends-on (:log4cl :drakma :do-urlencode :plump :lquery :lparallel :cl-ppcre :swank :ironclad :cl-base64 :cl-mongo))
