(defun isearch-insert-current-symbol ()
  (interactive)
  (let ((symbol (thing-at-point 'symbol t)))
    (if (not symbol)
        (message "No symbol at point")
      (let ((search-string (concat "\\_<" (regexp-quote symbol) "\\_>")))
        (setq isearch-regexp t
              isearch-regexp-function nil
              isearch-success t
              isearch-adjusted t
              isearch-string search-string
	      isearch-message search-string)
        (isearch-search-and-update)))))

(defun isearch-search-for-uuid ()
  (interactive)
  (let ((regexp "[a-fA-F0-9]\\{8\\}-\\([a-fA-F0-9]\\{4\\}-\\)\\{3\\}[a-fA-F0-9]\\{12\\}"))
    (setq isearch-regexp t
          isearch-regexp-function nil
          isearch-success t
          isearch-adjusted t
          isearch-string regexp
	  isearch-message regexp)
    (isearch-search-and-update)))

(keymap-set isearch-mode-map "C-x" 'isearch-insert-current-symbol)
(keymap-set isearch-mode-map "C-u" 'isearch-search-for-uuid)
