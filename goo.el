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
                       "\\*\\(.*\\)-%s\\*"
                       "\\\\*\\1-\\\\(.*\\\\)\\\\*"
                       (symbol-value goo-buffer-format)))
        (predicate-name (replace-regexp-in-string
                          "\\(.*\\)-format"
                          "match-\\1"
                          (format "%s" goo-buffer-format))))
    `(progn (defvar ,(intern regexp-var-name) ,regexp-value)
            (defun ,(intern predicate-name) (buffer)
              (string-match ,(intern regexp-var-name) (buffer-name buffer))))))

(defconst goo-sentence-buffer-format "*goo-sentence-%s*")
(defconst goo-word-buffer-format "*goo-word-%s*")

(buffer-format-to-regexp goo-sentence-buffer-format)
(buffer-format-to-regexp goo-word-buffer-format)

(defun get-sorted-sentence-buffers ()
  (sort (cl-loop for buffer in
		 (buffer-list)
		 when (match-goo-sentence-buffer buffer)
		 collect buffer)
	(lambda (buffer1 buffer2)
	  (let ((buffer1-number (progn (match-goo-sentence-buffer buffer1)
				       (string-to-number (match-string 1 (buffer-name buffer1)))))
		(buffer2-number (progn (match-goo-sentence-buffer buffer2)
				       (string-to-number (match-string 1 (buffer-name buffer2))))))
	    (< buffer1-number buffer2-number)))))

(defun sentence-buffer-summary (buffer)
  "Summarize sentence buffer for use with helm"
  (let ((sentence-contents (with-current-buffer buffer (buffer-substring-no-properties (point-min) (point-max)))))
    (message (format "%s:\n%s\n%s"
		     (buffer-name buffer)
		     sentence-contents
		     (slime-eval `(cl-user::format
				   nil "~{~12<~A~>: ~A...~^~%~}"
				   (cl-user::apply
				    (cl-user::function cl-user::append)
				    (cl-user::mapcar
				     (cl-user::lambda (word)
						      (cl-user::list
						       (goo::goo-word-reading word)
						       (cl-user::subseq
							(goo::simple-text-print word)
							0 20)))
				     (goo::goo-sentence-children
				      (goo::sentence-from-string ,sentence-contents))))))))))

(defun get-sentence-buffers-for-helm ()
  (mapcar 
   (lambda (buffer)
     (cons (sentence-buffer-summary buffer) buffer))
   (get-sorted-sentence-buffers)))

;; TODO: All sentence buffers must have associated sentences in the hashtable of SBCL right now...
;; TODO: This function allows me to very easily choose a sentence
;; buffer with helm, but what I actually want (and was working on
;; before) was a function that would just give me a high-level
;; overview of the sentence and its relation to the unknown words in
;; it. Is this better done in ELISP or Common Lisp? 
(defun show-sentence-buffers ()
  (let ((helm-source-goo-sentence-buffers `((name . "Goo sentence buffers")
					    (candidates . ,(get-sentence-buffers-for-helm))
					    (action . (lambda (candidate)
							(switch-to-buffer (cdr (assoc candidate data)))))
					    (multiline . 5))))

    (helm :sources 'helm-source-goo-sentence-buffers)))

(defun get-sentence-buffer-numbers ()
  ""
  (cl-loop for buffer in
	   (buffer-list)
	   when (match-goo-sentence-buffer buffer)
	   collect (string-to-number (match-string 1 (buffer-name buffer)))))

(defun new-sentence-buffer ()
  ""
  (interactive)
  (let ((available-number 1)
        (used-numbers (sort (get-sentence-buffer-numbers) '<)))
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
    (let ((result (slime-eval-buffer)))
      (switch-to-buffer original-buffer)
      result)))

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

