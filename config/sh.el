(defun sh-mode-customize ()
  (local-unset-key (kbd "C-c C-\\")))

(add-hook 'sh-mode-hook 'sh-mode-customize)
