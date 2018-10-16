(defun indent-tools-show-parent (&optional prev)
  (interactive)
  (save-excursion
    (if prev
        (goto-char prev))
    (indent-tools-goto-parent)
    (setq prev (point))
    (message (buffer-substring-no-properties
              (point)
              (save-excursion
                (end-of-line)
                (point)))))
  (lexical-let ((prev prev))
    (set-transient-map
     (let ((m (make-sparse-keymap)))
       (define-key m (kbd "U") (lambda ()
                                 (interactive)
                                 (indent-tools-show-parent prev)))
       m))))

(defun indent-tools-hook ()
  (define-key indent-tools-command-map "U" 'indent-tools-show-parent))

(add-hook 'indent-tools-minor-mode-hook 'indent-tools-hook)
