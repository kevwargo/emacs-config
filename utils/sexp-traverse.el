(defun sexp-traverse-goto-start ()
  (interactive)
  (when-let ((pos (nth 1 (syntax-ppss))))
    (goto-char pos)))

(defun sexp-traverse-goto-end ()
  (interactive)
  (if (sexp-traverse-goto-start)
      (forward-sexp)))

(defun nest--context ()
  (let (pos
        pos-list
        lines
        last-line-start
        messages-buffer-max-lines)
    (save-excursion
      (while (setq pos (nth 1 (syntax-ppss)))
        (push pos pos-list)
        (goto-char pos)))
    (save-excursion
      (dolist (pos pos-list)
        (goto-char pos)
        (unless (equal last-line-start (line-beginning-position))
          (setq last-line-start (line-beginning-position))
          (push (buffer-substring last-line-start (line-end-position)) lines))))
    (nreverse lines)))

(defun nest-context-show ()
  (interactive)
  (message "%s" (s-join "\n" (nest--context))))

(defun nest-context-copy ()
  (interactive)
  (kill-new (s-join " " (nest--context))))
