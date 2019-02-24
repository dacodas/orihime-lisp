(in-package :orihime)

(defparameter *connection* nil)
(defparameter *templates-directory* (merge-pathnames (make-pathname :directory '(:relative "static"))
                                     (asdf:system-source-directory :orihime)))

(defun make-text-hash (contents)
   (ironclad:digest-sequence :sha256 (sb-ext:string-to-octets contents)))

(defun initialize-connection ()
  (setf *connection*
        (dbi:connect :mysql
                     :host "mysql-container"
                     :database-name "orihime"
                     :username "sbcl"
                     :password "dacodastrack"))

  (let* ((query (dbi:prepare *connection*
                             "set names utf8;"))
         (result (dbi:execute query)))))

(defmacro defun/sql (function-name lambda-list &body body)
  `(defun ,function-name ,lambda-list
     (if (not *connection*)
         (initialize-connection))
     ,@body))

(defun join-sql-strings (&rest strings)
  (with-output-to-string (*standard-output*)
    (format t "~{~A~^ ~%~};" strings)))

(defparameter *sql-keys-default-special-cases*
  '(:|lcase(hex(definition_hash))| :definition-hash
    :|lcase(hex(words.definition_hash))| :definition-hash
    :|lcase(hex(hash))| :hash
    :|lcase(hex(texts.hash))| :hash))

(defun symbolize-sql-keys (sql-result &optional additional-special-cases)
  (let ((special-cases (copy-list *sql-keys-default-special-cases*)))
    (loop for (key value) on additional-special-cases by #'cddr
       do (setf (getf special-cases key) value))
    (loop for (key value) on sql-result
       by #'cddr
       append (list (intern (format nil "~A"
                                    (let ((special-case (getf special-cases key)))
                                      (if special-case
                                          special-case
                                          (cl-ppcre:regex-replace-all "_"  (string-upcase (symbol-name key)) "-"))))
                            :keyword)
                    value))))


(defmacro let-print% (let-name let-bindings &body body)
  `(,let-name ,let-bindings
              (format t "~%~%LET BINDINGS~%")
              ,@(loop for binding in let-bindings
                   collect
                     (let ((symbol (car binding)))
                       `(format t "    ~A: ~S~%"
                                ,(string-downcase (symbol-name symbol))
                                ,symbol)))
              (format t "END LET BINDINGS~%~%")
              ,@body))

(defmacro let*-print (let-bindings &body body)
  `(let-print% let* ,let-bindings ,@body))

(defmacro let-print (let-bindings &body body)
  `(let-print% let ,let-bindings ,@body))

(defmacro with-plist-properties (property-symbols plist &body body)
  (flet ((keyword-from-symbol (symbol)
           (intern (symbol-name symbol) :keyword)))
    (let ((plist-symbol (gensym)))
      `(let ((,plist-symbol ,plist))
         (symbol-macrolet ,(loop for property-symbol in property-symbols
                              collect
                                `(,property-symbol (getf ,plist-symbol ,(keyword-from-symbol property-symbol))))
           ,@body)))))

(defmacro process-sql-row (sql-response &body body)
  (let ((sql-response-symbol (gensym "SQL-RESPONSE-")))
    `(let* ((,sql-response-symbol ,sql-response)
            (row (symbolize-sql-keys (dbi:fetch ,sql-response-symbol))))
       ,@body)))

(defmacro process-multiple-sql-rows (sql-response &body body)
  (let ((sql-response-symbol (gensym "SQL-RESPONSE-")))
    `(let ((,sql-response-symbol ,sql-response))
       (loop for row = (symbolize-sql-keys (dbi:fetch ,sql-response-symbol))
          while row
          collect  
            ,@body))))

(defmacro mustache-render (&rest args)
  "Don't escape HTML"
  `(flet ((mustache::escape (string)
            (declare (type string string))
            string))
     (mustache:render ,@args)))
