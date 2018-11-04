(setq slime-enable-evaluate-in-emacs t)

(defun show-goo-word (word)
  ""
  (let ((word-buffer-name (format "*goo-word-%s*" word))
        (definition (slime-eval `(goo::get-word-definition ,word))))
    (switch-to-buffer word-buffer-name)
    (erase-buffer)
    (insert definition)
    (fill-individual-paragraphs (point-min) (point-max))
    (text-scale-set 4)
    (goto-char (point-min))
