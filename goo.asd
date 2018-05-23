;;;; goo.asd

(asdf:defsystem goo
  :description "Lispy interface to dictionary.goo.ne.jp"
  :serial t
  :components ((:file "package")
	       (:file "goo")
	       (:file "goo-page"))
  :depends-on (:log4cl :drakma :do-urlencode :plump :lquery :ironclad))
