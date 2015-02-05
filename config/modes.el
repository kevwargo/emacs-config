(require 'show-point-mode)
(require 'goto-last-change)
(require 'ido)
;; (require 'auto-complete-clang)


(add-to-list 'load-path (concat kec:config-dir "undo-tree"))
(add-to-list 'load-path (concat kec:config-dir "rules-editing-mode"))
(add-to-list 'load-path (concat kec:config-dir "php-mode-1.13.1"))

(load "pddl-mode")
(load "my-drools")

(defun enable-linum-in-some-buffers ()
  (unless (or (minibufferp)
              (derived-mode-p
               'comint-mode
               'help-mode
               'term-mode))
    (linum-mode 1)))

(ido-mode nil)
(column-number-mode t)
(show-point-mode t)
(delete-selection-mode t)
(and (boundp 'scroll-bar-mode)
     (scroll-bar-mode 0))

(autoload 'undo-tree-mode "undo-tree" "Enable undo-trees" t)
(autoload 'global-undo-tree-mode "undo-tree" "Enable undo-trees globally" t)
(setq undo-tree-enable-undo-in-region nil)
(global-undo-tree-mode t)

;; This need to be set BEFORE enabling winner
(setq winner-dont-bind-my-keys t)
(winner-mode t)

(setq show-paren-delay 0)
(show-paren-mode 1)
(electric-pair-mode 1)

(autoload 'php-mode "php-mode" "Major mode for editing php code." t)
(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))
(add-to-list 'auto-mode-alist '("\\.inc$" . php-mode))

(add-hook 'after-change-major-mode-hook 'enable-linum-in-some-buffers)
