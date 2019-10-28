(defun ido-all-matches ()
  (interactive)
  (setq ido-text ido-matches)
  (setq ido-exit 'done)
  (exit-minibuffer))

(defun ido-all-matches-hook ()
  (define-key ido-completion-map (kbd "C-o") 'ido-all-matches))

(add-hook 'ido-setup-hook 'ido-all-matches-hook)
