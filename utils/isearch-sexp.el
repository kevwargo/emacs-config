(defun insert-current-sexp ()
  (interactive)
  (let (sexp search-string)
    (if (not (setq sexp (thing-at-point 'sexp t)))
        (message "No sexp at point")
      (setq search-string (concat "\\<" sexp "\\>")
            isearch-regexp t
            isearch-word nil
            isearch-success t
            isearch-adjusted t)
      (isearch-process-search-string search-string search-string))))

(define-key isearch-mode-map (kbd "C-x") 'insert-current-sexp)
