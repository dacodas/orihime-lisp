(setq slime-enable-evaluate-in-emacs t)

;; All of these emacs functions should be completely agnostic to the
;; GOO underpinnings and should only work through the words interface

(setq *text-buffer-format* "*text-%s*")
(setq *text-buffer-regex* "\\*text-\\(.*\\)\\*")

(defun kill-buffer-text-id ()
  (interactive)
  (kill-new (get-buffer-text-id (buffer-name))))

(defun get-buffer-text-id (buffer-name)
  (interactive "b")
  (string-match *text-buffer-regex* buffer-name)
  (message (match-string-no-properties 1 buffer-name)))

(defun add-text-from-buffer ()
  (interactive)
  (let* ((text-contents (buffer-substring-no-properties (point-min) (point-max)))
         (text-id (slime-eval `(goo::text-id (goo::make-text ,text-contents)))))
    (switch-to-buffer (format *text-buffer-format* text-id))
    (insert text-contents)
    (setq buffer-read-only t)
    (goto-char (point-min))))

(defun show-goo-word (word)
  ""
  (slime-eval-async `(goo::get-word-definition-id ,word)
    (lambda (definition-text-id)
      (let ((word-buffer-name (format *text-buffer-format* definition-text-id))
            (definition (slime-eval `(goo::get-text-from-id ,definition-text-id))))
        (switch-to-buffer word-buffer-name)
        (erase-buffer)
        (insert definition)
        (fill-individual-paragraphs (point-min) (point-max))
        (text-scale-set 4)
        (goto-char (point-min))))))

(defun show-goo-word-from-region (start end &optional modify-text)
  (interactive "r")
  (let* ((buffer (current-buffer))
         (ocurrence (buffer-substring-no-properties start end))
         (reading (if modify-text
                      (read-string "Sequence to lookup: " ocurrence)
                    ocurrence))
         (text-id (get-buffer-text-id (buffer-name))))
    (if text-id
        (progn
          (message "Getting text for %s" text-id)
          (slime-eval-async `(goo::add-child-word-to-text ,text-id ,reading ,ocurrence)
            `(lambda (result) (show-goo-word ,reading))))
      (show-goo-word reading))))

(defun show-goo-word-from-region-and-modify (start end)
  (interactive "r")
  (show-goo-word-from-region start end t))
