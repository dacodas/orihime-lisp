;;; goo --- some bullshit, bruh

;;; Commentary:
;;; This is a package. It does stuff.

;;; Code:
;;; Here is some actual code

(setq slime-enable-evaluate-in-emacs t)

(defun show-goo-word (word text)
  ""
    (let ((original-buffer (current-buffer)))
      (switch-to-buffer (format "*goo-word-%s*" word))
      (insert text)
      (fill-individual-paragraphs (point-min) (point-max))
      (text-scale-adjust 4)
      (goto-char (point-min)))
    t)

(defun lookup-goo-word (word)
  ""
  (eval-slime
   `(goo:lookup-and-show-new-word ,word)))

(defun lookup-goo-word-prompt ()
  ""
  (interactive)
  (lookup-goo-word (read-string "What word to lookup? ")))

(defun lookup-goo-word-from-region (start end)
  ""
  (interactive "r")
  (lookup-goo-word (buffer-substring-no-properties start end)))

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

(defun word-from-buffer ()
  ""
  (interactive)
  (let ((buffer-name (format "%s" (current-buffer))))
    (string-match "\\*goo-word-\\(.*\\)\\*" buffer-name)
    (match-string 1 buffer-name)))

(defun fill-buffer ()
  ""
  
  )
