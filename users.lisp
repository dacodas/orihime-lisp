(defun/sql sql-authenticate-user (user-id password)
  (let* ((query (dbi:prepare *connection*
                             "SELECT salt WHERE USER = ?;")))
    (dbi:execute query user-id)))

(defun/sql sql-add-user (user-id password)
  (let* ((hash (hash user-id password))
         (query (dbi:prepare *connection*
                             "INSERT INTO users (user_id, salt, hash) VALUES (?, ?);")))
    (dbi:execute query user-id hash)))

(defun check-user-hash (user-id password)

  )

(defun generate-new-user-hash (user-id password)
  (with-output-to-string (output)
    (sb-ext:run-program "python3" `("/usr/local/src/python/orihime/hash.py" ,user-id ,password)
                        :output output)))
