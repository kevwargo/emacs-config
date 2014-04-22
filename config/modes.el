(require 'show-point-mode)
(require 'goto-last-change)
;; (require 'auto-complete-clang)


(column-number-mode t)
(show-point-mode t)
(delete-selection-mode t)
(scroll-bar-mode 0)
;; (global-linum-mode t)

(add-to-list 'load-path (concat kec:config-dir "undo-tree"))
(autoload 'undo-tree-mode "undo-tree" "Enable undo-trees" t)
(autoload 'global-undo-tree-mode "undo-tree" "Enable undo-trees globally" t)
(setq undo-tree-enable-undo-in-region nil)
(global-undo-tree-mode t)

;; This need to be set BEFORE enabling winner
(setq winner-dont-bind-my-keys t)
(winner-mode t)

(setq show-paren-delay 0)
(show-paren-mode 1)
