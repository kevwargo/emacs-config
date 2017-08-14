(defun js-mode-hook-custom ()
  (local-set-key (kbd "C-{") 'embrace-selected-lines))

(defun js-set-local-indent-level (level)
  (interactive "N")
  (setq-local js-indent-level level))

(add-hook 'js-mode-hook 'js-mode-hook-custom)
