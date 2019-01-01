(defmacro with-plist-properties (property-symbols plist &body body)
  (flet ((keyword-from-symbol (symbol)
           (intern (symbol-name symbol) :keyword)))
    (let ((plist-symbol (gensym)))
      `(let ((,plist-symbol ,plist))
         (symbol-macrolet ,(loop for property-symbol in property-symbols
                              collect
                                `(,property-symbol (getf ,plist-symbol ,(keyword-from-symbol property-symbol))))
           ,@body)))))

(flet ((perform-hash-check (sql-row)
         (let* ((contents (getf sql-row :contents))
                (old-hash (getf sql-row :definition-hash))
                (newly-computed-hash (string-downcase
                                     (with-output-to-string (*standard-output*)
                                       (loop for byte across (make-text-hash contents)
                                          do (format t "~2,'0x" byte))))))

           (format t "Comparing ~A and ~A~%" old-hash newly-computed-hash)
           (if (equal old-hash newly-computed-hash)
               (format t "All good!~%")
               (format t "Problemo!~%"))))
       (print-first-word-and-first-line (sql-row)

         (with-plist-properties (reading contents definition-hash id)
             sql-row
           (let* ((contents-peek (trim-and-replace-big-breaks (subseq contents 0 (min 10 (length contents)))))
                  (new-plist (list :reading reading
                                   :contents-peek contents-peek
                                   :definition-hash definition-hash 
                                   :id id)))

             (format t "(~{~S ~S~^~%~})~%" new-plist)))))

  (let* ((limit 100)
         (query (dbi:prepare *connection*
                             (join-sql-strings "SELECT words.id, words.reading, lcase(hex(words.definition_hash)), texts.contents FROM words"
                                               "INNER JOIN texts ON words.definition_hash=texts.hash"
                                               "ORDER BY words.id DESC"
                                               "LIMIT ?")))
         (result (dbi:execute query limit)))
    (loop
       initially (format t "'(")
       for row = (symbolize-sql-keys (dbi:fetch result))
       while row
       do (print-first-word-and-first-line row)
       finally (format t ")"))))


(let ((entries '((:READING "魚介類"
                  :actual-reading "魚介"
                  :CONTENTS-PEEK "ぎょ‐かい〔‐カイ｜"
                  :DEFINITION-HASH "8d105b0a2e49bf9fe2ad1542473523a0b7a9fc05ea2d78d87d51505db2d1f175")
                 ;; (:READING "大正期"
                 ;;  :actual-reading "死骸"
                 ;;  :CONTENTS-PEEK "し‐がい【死骸／×屍"
                 ;;  :DEFINITION-HASH "6fca5ad0c655020cdd4ff19b264a3a812ed6332e2e98ecbf2df2081d8e93429c")
                 ;; (:READING "死骸"
                 ;;  :actual-reading "傾向"
                 ;;  :CONTENTS-PEEK "けい‐こう〔‐カウ〕"
                 ;;  :DEFINITION-HASH "5387a96182e7c164ad799175686f6b2f9b26677fd1d14b2e63d311577001f635")
                 ;; (:READING "死骸"
                 ;;  :actual-reading "時点"
                 ;;  :CONTENTS-PEEK "じ‐てん【時点】の意"
                 ;;  :DEFINITION-HASH "9f1a6d84d144c42f59ade4e23588c6ee4bb0de8a6ee2a63f9e0e1d075fd4c5e9")
                 ;; (:READING "傾向"
                 ;;  :actual-reading "大勢"
                 ;;  :CONTENTS-PEEK "おお‐ぜい〔おほ‐〕"
                 ;;  :DEFINITION-HASH "3465e93ded2207e49c898ea4a540a35242f6ae1181f4b1c7cf448f64655c8d32")
                 ;; (:READING "大勢"
                 ;;  :actual-reading "最多"
                 ;;  :CONTENTS-PEEK "さい‐た【最多】の意"
                 ;;  :DEFINITION-HASH "42226e97e58befd95b4931e30b6e454a03df55076162dc4edfd8b060525002bb")
                 ;; (:READING "時点"
                 ;;  :actual-reading "及ぶ"
                 ;;  :CONTENTS-PEEK "およ・ぶ【及ぶ】の意"
                 ;;  :DEFINITION-HASH "968c406953e413154a60eddd38e8f8cd15ae40a4e78e691eb82b15d08a2ab381")
                 )))

  (loop for entry in entries
     do (with-plist-properties (actual-reading definition-hash contents-peek)
            entry

          (format t "Changing ~A to have text ~A~%" actual-reading contents-peek)

          (let* ((query (dbi:prepare *connection*
                                     (join-sql-strings "UPDATE words"
                                                       "SET definition_hash = unhex(?)"
                                                       "WHERE reading = ?")))
                 (result (dbi:execute query definition-hash actual-reading)))


            (format t "Got result ~A and ~A~%" result (dbi:fetch result))))))
