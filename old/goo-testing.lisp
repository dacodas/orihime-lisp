;; Exact hit 怠る
;; Only one results page 怠り
;; Many results　怠
;; Many results　い
(defparameter *words-to-initialize* (list "怠る" "怠り" "怠" "い"))

(defun initialized-words-temporary-format (word)
  (format nil "~A-response" word))

(defun initialized-words-global-format (word)
  (format nil "*~A-response*" word))

(defun initialized-words-temporary-variable (word)
  (intern (string-upcase (initialized-words-temporary-format word))))

(defun initialized-words-global-variable (word)
  (intern (string-upcase (initialized-words-global-format word))))

(defmacro uninitialize-words (words)
  (append '(progn)
          (loop for word in (symbol-value words)
             collect `(makunbound (quote ,(initialized-words-global-variable word))))))

(defmacro initialize-words (words)
  (loop for word in (symbol-value words)
     collecting `(,(initialized-words-temporary-variable word)
                (lparallel:future (multiple-value-list (grab-goo-response ,word))))
     into let-clauses
     collecting `(defvar ,(initialized-words-global-variable word)
                   (lparallel:force ,(initialized-words-temporary-variable word)))
     into defvar-clauses
     finally (return
               (append `(let ,let-clauses) defvar-clauses))))

(initialize-words *words-to-initialize*)

(defparameter *test-pages*
  (loop for word in *words-to-initialize*
     collect (symbol-value (initialized-words-global-variable word))))
