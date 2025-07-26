(require 'undo-tree)

(setq undo-tree-history-directory-alist
      `(("." . ,(concat user-emacs-directory "undo-tree-histories"))))

(setq undo-tree-enable-undo-in-region nil)
(global-undo-tree-mode)
