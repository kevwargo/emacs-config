(defun sexp-traverse-goto-start ()
  (interactive)
  (when-let ((pos (nth 1 (syntax-ppss))))
    (goto-char pos)))

(defun sexp-traverse-goto-end ()
  (interactive)
  (if (sexp-traverse-goto-start)
      (forward-sexp)))
