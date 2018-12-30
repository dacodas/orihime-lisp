;; Add auto_increment to the ID


(let ((reading "女房")
      (definition "にゅう‐ぼう〔ニウバウ〕【▽女房】の意味

「にょうぼう」に同じ。
「三十余人の―たちを始めとして」〈仮・恨の介・上〉
[補説]「にゅうぼう」と書いて「にょうぼう」の発音を表したもの。"))

  (insert-reading reading definition))

(let* ((query (dbi:prepare *connection*
                           "SELECT reading, hex(definition_hash) FROM words;"))
       (result (dbi:execute query)))

  (loop for row = (dbi:fetch result)
     while row
     do (format t "~S" (getf row :|hex(definition_hash)|))))

(let* ((query (dbi:prepare *connection*
                           "SELECT contents FROM texts WHERE hash = unhex(?);"))
       (result (dbi:execute query "E1D39ACC555278D28C3A532AA66D73C31FDE687577C7246D75D8B753EC43CE00")))

  (loop for row = (dbi:fetch result)
     while row
     do (format t "~S" row)))


(let* ((query (dbi:prepare *connection*
                           "select words.reading, texts.contents 
from words
inner join texts on words.definition_hash = texts.hash;"))
       (result (dbi:execute query)))

  (loop for row = (dbi:fetch result)
     while row
     do (format t "~S" row)))
