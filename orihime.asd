;;;; orihime.asd

(asdf:defsystem orihime
  :description "Lispy word webs"
  :serial t
  :components ((:file "package")
               (:file "utilities")
               (:file "orihime")
               (:file "results-paging")
               (:file "anki-cards")
               ;; (:file "graphs")
               (:module backends
                        :pathname "backends"
                        :components ((:file "goo-utils")
                                     (:file "goo-web")
                                     (:file "goo-local")
                                     (:file "larousse"))))
  :depends-on (:log4cl 
               :cl-who
               :cl-json
               :drakma 
               :do-urlencode 
               :plump 
               :lquery 
               :lparallel 
               :cl-ppcre 
               :ironclad 
               :cl-dbi
               :cl-containers 
               :cl-graph 
               :cl-mustache))
