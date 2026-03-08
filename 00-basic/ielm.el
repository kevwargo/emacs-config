(defun ielm-on-current-buffer ()
  (interactive)
  (let ((buf (current-buffer))
        (display-buffer-overriding-action '(nil . ((inhibit-same-window . t)))))
    (ielm)
    (ielm-change-working-buffer buf)))

(defun ielm-custom-hook ()
  (add-hook 'xref-backend-functions 'elisp--xref-backend nil t))

(add-hook 'ielm-mode-hook 'ielm-custom-hook)

(keymap-global-set "C-c z" 'ielm-on-current-buffer)
