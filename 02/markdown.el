(defun markdown-delete-whitespace-on-save ()
  (add-hook 'before-save-hook 'delete-trailing-whitespace nil t))

(add-hook 'markdown-mode-hook 'markdown-delete-whitespace-on-save)
