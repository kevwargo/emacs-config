(defun sh-mode-customize ()
  (mapc 'keymap-local-unset
        '("C-c C-\\" "C-c C-z" "C-c C-d")))

(add-hook 'sh-mode-hook 'sh-mode-customize)
