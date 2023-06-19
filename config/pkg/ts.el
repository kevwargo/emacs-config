(defun ts-mode-keymap-modify ()
  (local-set-key (kbd "C-{") 'embrace-selected-lines))

(add-hook 'typescript-mode-hook 'ts-mode-keymap-modify)
(add-hook 'typescript-mode-hook 'lsp)
