(defun asm-period ()
  "Insert a period; if there's no non-blank characters before it, delete the indentation beforre."
  (interactive)
  (save-excursion
    (skip-syntax-backward " ")
    (when (bolp)
      (delete-horizontal-space)))
  (call-interactively 'self-insert-command))

(define-key asm-mode-map (kbd ".") 'asm-period)
