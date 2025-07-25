(require 'ido)

(defun ido-all-matches ()
  (interactive)
  (setq ido-text ido-matches)
  (setq ido-exit 'done)
  (exit-minibuffer))

(defun ido-custom-keys-setup ()
  (define-key ido-completion-map (kbd "C-o") 'ido-all-matches))

(add-hook 'ido-setup-hook 'ido-custom-keys-setup)

(ido-mode 1)
