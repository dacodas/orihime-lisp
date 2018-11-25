(setq slime-enable-evaluate-in-emacs t)

(setq *text-buffer-format* "*text-%s*")
(setq *text-buffer-regex* "\\*text-\\(.*\\)\\*")


(defun orihime-get-buffer-text-id (buffer-name)
  (interactive "b")
  (if (string-match *text-buffer-regex* buffer-name)
      (message (match-string-no-properties 1 buffer-name))))

(defun orihime-add-text-from-buffer ()
  (interactive)
  (let* ((text-contents (buffer-substring-no-properties (point-min) (point-max)))
         (text-id (slime-eval `(orihime::text-id (orihime::make-text ,text-contents)))))
    (switch-to-buffer (format *text-buffer-format* text-id))
    (insert text-contents)
    (setq buffer-read-only t)
    (goto-char (point-min))))

(defun orihime-kill-buffer-text-id ()
  (interactive)
  (kill-new (orihime-get-buffer-text-id (buffer-name))))

(defun orihime-show-word (word)
  (interactive "sWord to lookup: ")
  (slime-eval-async `(orihime::word-definition-text-id ,word)
    (lambda (definition-text-id)
      (let ((word-buffer-name (format *text-buffer-format* definition-text-id))
            (definition (slime-eval `(orihime::get-text-from-id ,definition-text-id))))
        (switch-to-buffer word-buffer-name)
        (erase-buffer)
        (insert definition)
        (fill-individual-paragraphs (point-min) (point-max))
        (text-scale-set 4)
        (goto-char (point-min))))))

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
          (slime-eval-async `(orihime::add-child-word-to-text ,text-id ,reading ,ocurrence)
            `(lambda (result) ()))))

    (orihime-show-word reading)))

(defun orihime-show-word-from-region-and-modify (start end)
  (interactive "r")
  (orihime-show-word-from-region start end t))
