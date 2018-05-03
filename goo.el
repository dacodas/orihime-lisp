(setq slime-enable-evaluate-in-emacs t)

(defun show-japanese-word (word text)
    (let ((original-buffer (current-buffer)))
      (switch-to-buffer (format "*japanese-word-%s*" word))
      (insert text)
      ;; (switch-to-buffer original-buffer)
      )
    t)

(defun lookup-japanese-word (word)
  (eval-slime
   `(lookup-and-show-new-word ,word)))

(defun lookup-japanese-word-prompt ()
  (interactive)
  (lookup-japanese-word (read-string "What word to lookup? ")))

(defun lookup-japanese-word-marked (start end)
  (interactive "r")
  (lookup-japanese-word (buffer-substring-no-properties start end)))

(defun eval-slime (sexp) 
  (let ((original-buffer (current-buffer))
	(new-buffer-name (make-temp-name "*slime-eval*"))) 
    (switch-to-buffer new-buffer-name)
    (insert (format "%S" sexp))
    (slime-eval-buffer)
    (switch-to-buffer original-buffer)))

(defun word-from-buffer ()
  (interactive)
  (let ((buffer-name (format "%s" (current-buffer))))
    (string-match "\\*japanese-word-\\(.*\\)\\*" buffer-name)
    (match-string 1 buffer-name)))

