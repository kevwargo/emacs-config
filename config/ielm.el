(defun ielm-custom-hook ()
  (local-set-key (kbd "C-c C-f") 'find-function))

(add-hook 'ielm-mode-hook 'ielm-custom-hook)
