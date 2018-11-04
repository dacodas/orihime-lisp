(setq slime-enable-evaluate-in-emacs t)

(defun show-goo-word (word)
  ""
  (slime-eval-async `(goo::get-word-definition-id ,word)
    (lambda (definition-text-id)
      (let ((word-buffer-name (format "*text-%s*" definition-text-id))
            (definition (slime-eval `(goo::get-text-from-id ,definition-text-id))))
        (switch-to-buffer word-buffer-name)
        (erase-buffer)
        (insert definition)
        (fill-individual-paragraphs (point-min) (point-max))
        (text-scale-set 4)
        (goto-char (point-min))))))
