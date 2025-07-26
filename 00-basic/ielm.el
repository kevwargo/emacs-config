(defun ielm-custom-hook ()
  (add-hook 'xref-backend-functions 'elisp--xref-backend nil t))

(add-hook 'ielm-mode-hook 'ielm-custom-hook)
