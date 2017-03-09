(defun jde-keymap-modify ()
  (local-set-key (kbd "(") 'self-insert-command)
  (local-set-key (kbd ")") 'self-insert-command)
  (local-set-key (kbd "C-c C-v g") 'jde-wiz-get-set-methods)
  (local-set-key (kbd "C-<left>") 'jde-beginning-of-camel-tok)
  (local-set-key (kbd "C-<right>") 'jde-end-of-camel-tok)
  (local-set-key (kbd "C-c C-v l") 'jde-load-project-file)
  (local-set-key (kbd "C-c r") 'jde-compile&run))


(add-hook 'jde-mode-hook 'jde-keymap-modify)
;; (add-hook 'jde-mode-hook 'ajc-java-complete-mode)
