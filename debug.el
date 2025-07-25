(defun log-command-keys ()
  (scratch-log "%S %S" this-command (key-description (this-command-keys))))

(defun toggle-log-command-keys ()
  (interactive)
  (cond
   ((memq 'log-command-keys (default-value 'post-command-hook))
    (remove-hook 'post-command-hook 'log-command-keys)
    (message "Command keys logging disabled"))
   (t
    (add-hook 'post-command-hook 'log-command-keys)
    (message "Command keys logging enabled"))))

(keymap-global-set "C-x G" 'toggle-log-command-keys)
