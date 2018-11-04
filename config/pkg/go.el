(require 'go-guru)

(defun go-hook ()
  (go-set-project)
  (local-set-key (kbd "C-c C-f") 'gofmt)
  (local-set-key (kbd "C-c C-c") 'go-goto-map)
  (local-set-key (kbd "M-.") 'godef-jump)
  (local-set-key (kbd "M->") 'godef-jump-other-window)
  (local-set-key (kbd "C-{") 'embrace-selected-lines))

(add-hook 'go-mode-hook 'go-hook)
(add-hook 'before-save-hook 'gofmt-before-save)
