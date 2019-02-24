(defvar *orihime-backend* :goo-local)

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

(defun orihime-open-text-in-new-buffer (text-plist)
  (let* ((hash (or (plist-get text-plist :definition-hash)
                   (plist-get text-plist :hash)))
         (assertion (cl-assert (not (null hash)) nil "Unable to find hash in %s" text-plist))
         (word-buffer-name (format *text-buffer-format* hash))
         (definition (plist-get text-plist :contents)))
    (switch-to-buffer word-buffer-name)
    (erase-buffer)
    (insert definition)
    (fill-individual-paragraphs (point-min) (point-max))
    (text-scale-set 4)
    (setq buffer-read-only t)
    (goto-char (point-min))))

;; Grab the definition of the word, regardless of whether it has been searched for already 
(defun orihime-show-word (word)
  (interactive "sWord to lookup: ")
  (slime-eval-async
      `(cl-user::let ((orihime::*current-backend* ,*orihime-backend*))
         (orihime::find-word-and-definition ,word))
    #'orihime-open-text-in-new-buffer))

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
      (slime-eval-async
          `(cl-user::let ((orihime::*current-backend* ,*orihime-backend*))
             (orihime::add-child-word-to-text ,text-id ,reading ,ocurrence))
        `(lambda (result) (orihime-show-word ,reading)))
      (orihime-show-word reading))))

(defun orihime-show-word-from-region-and-modify (start end)
  (interactive "r")
  (orihime-show-word-from-region start end t))

(defun orihime-show-word-from-region-with-user-definition (start end)
  (interactive "r")
  (let ((*orihime-backend* :user-supplied))
    (orihime-show-word-from-region start end)))

(defun orihime-show-word-from-region-and-modify-with-user-definition (start end)
  (interactive "r")
  (let ((*orihime-backend* :user-supplied))
    (orihime-show-word-from-region start end t)))

(cl-defun orihime-open-recent-text (&optional (limit 20))
  (interactive)
  (cl-flet ((transformer (candidates)
                         (cl-loop for candidate in candidates
                                  collect `(,(plist-get candidate :contents) . ,candidate))))
    (helm :sources (helm-build-sync-source "*orihime-recent-texts*"
                     :candidates (slime-eval `(orihime::most-recent-texts ,limit))
                     :candidate-transformer #'transformer 
                     :action #'orihime-open-text-in-new-buffer
                     :multiline t)
          :buffer "*helm sync source*")))
