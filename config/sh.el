(defun sh-mode-customize ()
  (local-unset-key (kbd "C-c C-\\"))
  (local-unset-key (kbd "C-c C-d")))

(add-hook 'sh-mode-hook 'sh-mode-customize)
