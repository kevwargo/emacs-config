(defun js-mode-hook-custom ()
  (local-set-key (kbd "C-{") 'embrace-selected-lines)
  (setq indent-tabs-mode t))

(add-hook 'js-mode-hook 'js-mode-hook-custom)
