(defun ielm-custom-hook ()
  (local-set-key (kbd "C-c C-f") 'find-function)
  (add-hook 'xref-backend-functions 'elisp--xref-backend nil t))

(add-hook 'ielm-mode-hook 'ielm-custom-hook)
