(require 'show-point-mode)

;(require 'auto-complete-clang)

(column-number-mode t)
(show-point-mode t)
(global-undo-tree-mode t)
;; (global-linum-mode t)
(delete-selection-mode t)
(scroll-bar-mode 0)

;; This need to be set BEFORE enabling winner
(setq winner-dont-bind-my-keys t)
(winner-mode t)

(show-paren-mode 1)
