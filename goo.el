;;; goo --- some bullshit, bruh

;;; Commentary:
;;; This is a package. It does stuff.

;;; Code:
;;; Here is some actual code

(setq slime-enable-evaluate-in-emacs t)

(defun show-goo-word (word text)
  ""
    (switch-to-buffer (format "*goo-word-%s*" word))
    (insert text)
    (fill-individual-paragraphs (point-min) (point-max))
    (text-scale-adjust 4)
    (goto-char (point-min))
    t)


(defmacro buffer-format-to-regexp (goo-buffer-format)
  "Generate regexp variable and matching function from GOO-BUFFER-FORMAT.
- A variable goo-.*-regexp which allows for matching the buffer
- A function match-goo-.* which matches a buffer and returns the
  \"string-match\" result"
  (let ((regexp-var-name (replace-regexp-in-string
                          "\\(.*\\)-format"
                          "\\1-regexp"
                          (format "%s" goo-buffer-format)))
        (regexp-value (replace-regexp-in-string
                       "\\*\(.*\)-%s\\*"
                       "\\\\*\\1-\\(.*\\)\\\\*"
                       (symbol-value goo-buffer-format)))
        (predicate-name (replace-regexp-in-string
                          "\\(.*\\)-format"
                          "match-\\1"
                          (format "%s" goo-buffer-format))))
    `(progn (defvar ,(intern regexp-var-name) ,regexp-value)
            (defun ,(intern predicate-name) (buffer)
              (string-match ,(intern regexp-var-name) (buffer-name buffer))))))

(defvar goo-sentence-buffer-format "*goo-sentence-%s*")
(defvar goo-word-buffer-format "*goo-word-%s*")

(buffer-format-to-regexp goo-sentence-buffer-format)
(buffer-format-to-regexp goo-word-buffer-format)

(defun get-sentence-buffer-numbers ()
  ""
  (sort (cl-loop for buffer in
                 (buffer-list)
                 when (match-goo-sentence-buffer buffer)
                 collect (string-to-number (match-string 1 (buffer-name buffer))))
        '<))

(defun new-sentence-buffer ()
  ""
  (let ((available-number 1)
        (used-numbers (get-sentence-buffer-numbers)))
    (cl-loop while (eq available-number (car used-numbers))
             do (progn
                  (cl-incf available-number)
                  (setf used-numbers (cdr used-numbers))))
    (switch-to-buffer (format "*goo-sentence-%s*" available-number))))

(defun eval-slime (sexp)
  "Take SEXP and evaluate it in slime.
    
Note: I'm not entirely sure why slime-eval doesn't work in my
case. Running it gives some bullshit"
  (let ((original-buffer (current-buffer))
	(new-buffer-name (make-temp-name "*slime-eval*")))
    (switch-to-buffer new-buffer-name)
    (insert (format "%S" sexp))
    (slime-eval-buffer)
    (switch-to-buffer original-buffer)))

(defun add-sentence (start end)
  (interactive "r")
  (let ((buffer-contents (buffer-substring-no-properties (point-min) (point-max)))
        (user-response (read-string "Are you sure? (This will make the buffer read-only): ")))
    (if (string-match "y" user-response)
        (progn
          (eval-slime `(goo:add-sentence ,buffer-contents))
          (setq buffer-read-only t))
      (message "Not adding this sentence!"))))

(cl-defun lookup-goo-word (word &key parent-word parent-sentence)
  ""
  (eval-slime
   `(goo:lookup-and-show-new-word ,word :parent-word ,parent-word :parent-sentence ,parent-sentence)))

(defun lookup-goo-word-prompt ()
  ""
  (interactive)
  (lookup-goo-word (read-string "What word to lookup? ")))

(defun lookup-goo-word-from-region (start end)
  ""
  (interactive "r")
  (let ((buffer (current-buffer))
        (goo-word (buffer-substring-no-properties start end)))
    (cond ((match-goo-sentence-buffer buffer)
           (lookup-goo-word goo-word :parent-sentence (buffer-substring-no-properties (point-min) (point-max))))
          ((match-goo-word-buffer buffer)
           (lookup-goo-word goo-word :parent-word (match-string 1 (buffer-name buffer))))
          (t (lookup-goo-word goo-word)))))

(provide 'goo)
;;; goo.el ends here


