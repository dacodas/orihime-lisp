(in-package :orihime)

(log:config :debug)

(defparameter *current-backend* :goo-local)

(defparameter *connection*
  (dbi:connect :mysql
               :host "mysql-container"
               :database-name "orihime"
               :username "sbcl"
               :password "dacodastrack"))

(let* ((query (dbi:prepare *connection*
               "set names utf8;"))
      (result (dbi:execute query))))

;; (defun get-text-peek (text)
;;   (format nil "~A..."
;;           (let* ((contents (text-contents text))
;;                  (peek (subseq contents 0 (min 10 (length contents)))))
;;             (trim-and-replace-big-breaks peek))))

;; (defun get-text-child-words-peek (text)
;;   (let ((child-words (text-child-words text)))
;;     (loop for index below (min 5 (length child-words))
;;        collect (word-reading (aref child-words index)))))

(defun make-text-hash (contents)
   (ironclad:digest-sequence :sha256 (sb-ext:string-to-octets contents)))

(defun sql-add-text (content)
  (let* ((text-hash (make-text-hash content))
         (text-hash-hex (string-downcase (with-output-to-string (*standard-output*)
                                           (loop for byte across text-hash
                                              do (format t "~2,'0x" byte)))))
         (query (dbi:prepare *connection*
                             "INSERT INTO texts (hash, contents) VALUES (unhex(?), ?);")))
    (dbi:execute query text-hash-hex content)
    (values-list (list text-hash-hex text-hash))))

(defun sql-add-word (reading definition-text)
  (let* ((definition-text-hash (sql-add-text definition-text))
         (query (dbi:prepare *connection*
                                  "INSERT INTO words (reading, definition_hash) VALUES (?, unhex(?));")))
    (dbi:execute query reading definition-text-hash)))

(defun sql-add-text-child-word (text-hash reading start end)

  (add-word reading)

  (let ((query (dbi:prepare *connection*
                            "INSERT INTO text_child_words (text_id, word_id, beginning, ending) VALUES (?, ?, ?, ?);"))
        (text-id (sql-text-id-from-hash text-hash))
        (word-id (sql-word-id-from-reading reading)))
    (dbi:execute query text-id word-id start end)))

(defun join-sql-strings (&rest strings)
  (with-output-to-string (*standard-output*)
    (format t "~{~A~^ ~%~};" strings)))

(defun sql-word-and-text-from-reading (reading)
  (let* ((query (dbi:prepare *connection*
                             (join-sql-strings "SELECT words.reading, lcase(hex(words.definition_hash)), texts.contents FROM words"
                                               "INNER JOIN texts ON words.definition_hash=texts.hash"
                                               "WHERE words.reading = ?"
                                               ))))
    (dbi:fetch (dbi:execute query reading))))

(defun sql-definition-text-hash-from-reading (reading)
  (let* ((query (dbi:prepare *connection*
                             "SELECT definition_hash FROM words WHERE reading = ?;"))
         (result (dbi:execute query reading)))
    (getf (dbi:fetch result) :|definition_hash|)))

(defun sql-word-id-from-reading (reading)
  (let* ((query (dbi:prepare *connection*
                             "SELECT id FROM words WHERE reading = ?;"))
         (result (dbi:execute query reading)))
    (getf (dbi:fetch result) :|id|)))

(defun sql-text-id-from-hash (text-hash)
  (let* ((query (dbi:prepare *connection*
                             "SELECT id FROM texts WHERE hash = unhex(?);"))
         (result (dbi:execute query text-hash)))
    (getf (dbi:fetch result) :|id|)))

(defun add-child-word-to-text (text-hash reading ocurrence-in-text)
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

(defun find-definition-from-backend (reading)
  (case *current-backend*
    (:goo-web (goo-definition-from-meaning-page (goo-web-search reading)))
    (:goo-local (goo-definition-from-meaning-page (goo-local-search reading)))))

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

(defun symbolize-sql-keys (sql-result)
  (let ((special-cases '(:|lcase(hex(words.definition_hash))| :definition-hash)))
    (loop for (key value) on sql-result
       by #'cddr
       append (list (intern (format nil "~A"
                                    (let ((special-case (getf special-cases key)))
                                      (if special-case
                                          special-case
                                          (string-upcase (symbol-name key)))))
                            :keyword)
                    value))))

(defun add-word (reading)
  (let* ((definition (find-definition-from-backend reading)))
    (sql-add-word reading definition)))
