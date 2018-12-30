(setq slime-enable-evaluate-in-emacs t)

(setq *text-buffer-format* "*text-%s*")
(setq *text-buffer-regex* "\\*text-\\(.*\\)\\*")

(defun orihime-get-buffer-text-id (buffer-name)
  (interactive "b")
  (if (string-match *text-buffer-regex* buffer-name)
      (message (match-string-no-properties 1 buffer-name))))

;; Have CL orihime add the text to the database and return the resultant hash 
(defun orihime-add-text-from-buffer ()
  (interactive)
  (orihime-add-text-from-region (point-min) (point-max)))

(defun orihime-add-text-from-region (start end)
  (interactive "r")
  (let* ((text-contents (buffer-substring-no-properties start end))
         (text-id (slime-eval `(orihime::sql-add-text ,text-contents))))
    (switch-to-buffer (format *text-buffer-format* text-id))
    (insert text-contents)
    (setq buffer-read-only t)
    (goto-char (point-min))))

(defun orihime-kill-buffer-text-id ()
  (interactive)
  (kill-new (orihime-get-buffer-text-id (buffer-name))))

;; Grab the definition of the word, regardless of whether it has been searched for already 
(defun orihime-show-word (word)
  (interactive "sWord to lookup: ")
  (slime-eval-async `(orihime::find-word-and-definition ,word)
    (lambda (result)
      (let ((word-buffer-name (format *text-buffer-format* (plist-get result :definition-hash)))
            (definition (plist-get result :contents)))
        (switch-to-buffer word-buffer-name)
        (erase-buffer)
        (insert definition)
        (fill-individual-paragraphs (point-min) (point-max))
        (text-scale-set 4)
        (goto-char (point-min))))))

;; Should paging perhaps be done in emacs? That way we could use helm and all
;; sorts of nice beautiful things. Also, we would wouldn't have to use this
;; stupid hack with slime-eval async
(defun orihime-show-word-from-region (start end &optional modify-text)
  (interactive "r")
  (let* ((ocurrence (buffer-substring-no-properties start end))
         (reading (if modify-text
                      (read-string "Sequence to lookup: " ocurrence)
                    ocurrence))
         (text-id (orihime-get-buffer-text-id (buffer-name))))
    (if text-id
        (progn
          (message "Getting text for %s" text-id)
          (cl-macrolet ((stupid-macro ()
                                      `(slime-eval-async `(orihime::add-child-word-to-text ,text-id ,reading ,ocurrence)
                                         (lambda (result) (orihime-show-word ,reading)))))
            (stupid-macro)))
      (orihime-show-word reading))))

(defun orihime-show-word-from-region-and-modify (start end)
  (interactive "r")
  (orihime-show-word-from-region start end t))
