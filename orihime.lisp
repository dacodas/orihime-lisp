(in-package :orihime)

(log:config :debug)

(defparameter *current-backend* :goo-local)

(defun/sql sql-add-text (content)
  (let* ((text-hash (make-text-hash content))
         (text-hash-hex (string-downcase (with-output-to-string (*standard-output*)
                                           (loop for byte across text-hash
                                              do (format t "~2,'0x" byte)))))
         (query (dbi:prepare *connection*
                             "INSERT INTO texts (hash, contents) VALUES (unhex(?), ?);")))
    (restart-case (dbi:execute query text-hash-hex content)
      (skip-sql-text-insertion ()
        :report "Continue on without inserting this text into database"
        nil)
      (skip-sql-text-insertion ()
        :report "Replace the previous text with this one"
        nil))
    (values-list (list text-hash-hex text-hash))))

(defun/sql sql-add-word (reading definition-text)
  (let* ((definition-text-hash (sql-add-text definition-text))
         (query (dbi:prepare *connection*
                                  "INSERT INTO words (reading, definition_hash) VALUES (?, unhex(?));")))
    (dbi:execute query reading definition-text-hash)))

(defun/sql sql-add-text-child-word (text-hash reading start end)

  (add-word reading)

  (let ((query (dbi:prepare *connection*
                            "INSERT INTO text_child_words (text_id, word_id, beginning, ending) VALUES (?, ?, ?, ?);"))
        (text-id (sql-text-id-from-hash text-hash))
        (word-id (sql-word-id-from-reading reading)))
    (dbi:execute query text-id word-id start end)))

(defun/sql sql-word-and-text-from-reading (reading)
  (let* ((query (dbi:prepare *connection*
                             (join-sql-strings "SELECT words.reading, lcase(hex(words.definition_hash)), texts.contents FROM words"
                                               "INNER JOIN texts ON words.definition_hash=texts.hash"
                                               "WHERE words.reading = ?"
                                               ))))
    (dbi:fetch (dbi:execute query reading))))

(defun/sql sql-definition-text-hash-from-reading (reading)
  (let* ((query (dbi:prepare *connection*
                             "SELECT definition_hash FROM words WHERE reading = ?;"))
         (result (dbi:execute query reading)))
    (getf (dbi:fetch result) :|definition_hash|)))

(defun/sql sql-word-id-from-reading (reading)
  (let* ((query (dbi:prepare *connection*
                             "SELECT id FROM words WHERE reading = ?;"))
         (result (dbi:execute query reading)))
    (getf (dbi:fetch result) :|id|)))

(defun/sql sql-text-id-from-hash (text-hash)
  (let* ((query (dbi:prepare *connection*
                             "SELECT id FROM texts WHERE hash = unhex(?);"))
         (result (dbi:execute query text-hash)))
    (getf (dbi:fetch result) :|id|)))

(defun/sql sql-text-hash-from-id (text-id)
  (let* ((query (dbi:prepare *connection*
                             "SELECT hex(hash) as hash FROM texts WHERE id = ?;"))
         (result (dbi:execute query text-id)))
    (getf (dbi:fetch result) :|hash|)))

(defun/sql add-child-word-to-text (text-hash reading ocurrence-in-text)
  (let* ((text-query (dbi:prepare *connection* "SELECT id, contents FROM texts WHERE hash = unhex(ucase(?));"))
         (single-result (dbi:fetch (dbi:execute text-query text-hash))))
    (assert (not (null single-result))
            nil
            "No result for hash ~A" text-hash)
    (let ((text-id (getf single-result :|id|))
          (text-contents (getf single-result :|contents|)))
      (multiple-value-bind (start end)
          (cl-ppcre:scan ocurrence-in-text text-contents)
        (sql-add-text-child-word text-hash reading start end))))
  t)

(defun text-from-hash (text-hash)
  (second (dbi:fetch (orihime::grab-text (orihime::sql-text-id-from-hash text-hash)))))

(defun/sql most-recent-texts (&optional (limit 5))
  (let* ((query (dbi:prepare *connection* "select contents, id, lcase(hex(hash)) from texts ORDER BY id DESC LIMIT ?;"))
         (result (dbi:execute query limit)))
    (loop for row = (dbi:fetch result)
       while row
       collect (symbolize-sql-keys row))))


(defun/sql grab-definition-text-id-from-word-id (word-id)
  (let* ((query (dbi:prepare *connection*
                             (join-sql-strings
                              "SELECT texts.id FROM texts"
                              "INNER JOIN words"
                              "ON words.definition_hash = texts.hash"
                              "WHERE words.id = ?")))
         (result (dbi:execute query word-id)))

    (let ((return-value))
      (loop for row = (symbolize-sql-keys (dbi:fetch result))
         while row do
           (progn
             (format t "~S~%" row)
             (setf return-value (getf row :id)))
         finally (return return-value)))))

(defun/sql grab-child-words (text-id)
  (let* ((query (dbi:prepare *connection*
                             (join-sql-strings
                              "SELECT text_child_words.*, words.reading AS reading"
                              "FROM text_child_words"
                              "INNER JOIN words ON words.id = text_child_words.word_id"
                              "WHERE text_id = ?"
                              "ORDER BY text_child_words.beginning ASC")))
         (result (dbi:execute query text-id)))
    result))

(defun/sql grab-text (text-id)
  (let* ((query (dbi:prepare *connection*
                             (join-sql-strings
                              "SELECT contents, replace(substring(texts.contents, 1, 15), '\n', '') as peek FROM texts"
                              "WHERE id = ?")))
         (result (dbi:execute query text-id)))
    result))

(defun find-definition-from-backend (reading)
  (case *current-backend*
    (:goo-web (goo-definition-from-meaning-page (goo-web-search reading)))
    (:goo-local (goo-definition-from-meaning-page (goo-local-search reading)))
    (:larousse (larousse-search reading))
    (:user-supplied
     (progn
       (format t "Enter the definition please:~%")
       (let ((lines (loop for line = (read-line)
                       for (match groups) = (multiple-value-list 
                                             (cl-ppcre:scan-to-strings "(.*)DACODA" line))
                       while (not match)
                       collect line into lines
                       finally (return (append lines (list (aref groups 0)))))))
         (format nil "~{~A~^~%~}" lines))))
    (t (error "Backend ~A is not defined" *current-backend*))))

;; Grab the words and definition from the db or fill the db with a new word
(defun find-word-and-definition (reading)
  (labels ((helper (reading)
             (let ((word-and-text (symbolize-sql-keys (sql-word-and-text-from-reading reading))))
               (if (not word-and-text)
                   (progn
                     (add-word reading)
                     (helper reading))
                   word-and-text))))
    (helper reading)))

(defun add-word (reading)
  (let* ((definition (find-definition-from-backend reading)))
    (sql-add-word reading definition)))
