(defparameter *todo-list*
  '((:todo :tagline "Gameplan"
     (:p "Change database schema to support users")
     (:p "Get authentication working")
     (:p "Sanitize input from third-parties")
     (:p "Separate pods mysql, hunchentoot, and orihime pods out; expose them as services with selectors based on environment")
     (:p "Deal with multiple selections"))

    (:todo :tagline "Adding sentence and then changing it later" 
     (:p "How should I deal with updating the notes and cards after editing the sentence?")
     (:p "For example the following sentence. I would like to add 人一倍 to the list of child words.")
     (:p "けれども邪悪に対しては、人一倍に敏感であった。"))

    (:todo :tagline "Generate the inverse (block word in context and guess from definition/context)"
     (:p "This would be helpful for the following circumstance.")
     (:p "繁殖して頭数が増えていくことが危惧されている。")
     (:p "I want to thing of the word 危惧 in this context, so a card like the following would be helpful")
     (:p "繁殖して頭数が増えていくことが[...]されている。"))))

(with-open-file (output-readme
                 "README.md"
                 :direction :output
                 :if-exists :supersede
                 :if-does-not-exist :create)

  (format output-readme "# Orihime~%~%")
  (format output-readme "## To-do list~%~%")

  (loop for todo in *todo-list* do
       (let ((tagline (third todo))
             (content (nthcdr 3 todo)))
         (format output-readme "### ~A~%~%"
                 tagline)
         (loop for p in content
            do (format output-readme "~A~%~%" (second p))))))
