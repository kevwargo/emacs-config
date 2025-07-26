(defun makefile-comment-region (beg end &optional arg)
  (interactive)
  (save-excursion
    (goto-char beg)
    (beginning-of-line)
    (while
        (progn
          (insert ?#)
          (end-of-line)
          (not (or (eobp)
                   (> (1+ (point)) end)
                   (progn (forward-line) nil)))))))

(defun customize-makefile-mode ()
  (keymap-local-unset "C-c C-\\")
  (setq-local comment-region-function 'makefile-comment-region))

(add-hook 'makefile-mode-hook 'customize-makefile-mode)
