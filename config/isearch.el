(defun insert-current-symbol ()
  (interactive)
  (let ((symbol (substring-no-properties (thing-at-point 'symbol))))
    (if (not symbol)
        (message "No symbol at point")
      (let ((search-string (concat "\\_<" (regexp-quote symbol) "\\_>")))
        (setq isearch-regexp t
              isearch-word nil
              isearch-success t
              isearch-adjusted t)
        (isearch-process-search-string search-string search-string)))))

(defun isearch-findgrep ()
  (interactive)
  (let ((findgrep-regexp isearch-string))
    (isearch-exit)
    (call-interactively 'findgrep)))

(define-key isearch-mode-map (kbd "C-x") 'insert-current-symbol)
(define-key isearch-mode-map (kbd "C-S-f") 'isearch-findgrep)
