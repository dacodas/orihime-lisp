;;;; orihime.asd

(asdf:defsystem orihime
  :description "Lispy word webs"
  :serial t
  :components ((:file "package")
               (:file "orihime")
               (:file "results-paging")
               ;; (:file "serialization")
               ;; (:file "graphs")
               (:module backends
                        :pathname "backends"
                        :components ((:file "goo-utils")
                                     (:file "goo-web")
                                     (:file "goo-local"))))
  :depends-on (:log4cl 
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
