(require 'show-point-mode)

(add-to-list 'load-path (concat kec:config-dir "rules-editing-mode"))

;(load "pddl-mode")
;(load "my-drools")

(defun enable-linum-in-some-buffers ()
  (or (minibufferp)
      (derived-mode-p
       'comint-mode
       'help-mode
       'term-mode)
      (linum-mode 1)))

(global-subword-mode)

(column-number-mode t)
(show-point-mode t)
(delete-selection-mode t)
(and (boundp 'scroll-bar-mode)
     (scroll-bar-mode 0))

(global-auto-revert-mode t)

;; This need to be set BEFORE enabling winner
(setq winner-dont-bind-my-keys t)
(winner-mode t)

(setq show-paren-delay 0)
(show-paren-mode 1)
(electric-pair-mode 1)

(add-to-list 'auto-mode-alist '("\\.less$" . css-mode))
(add-to-list 'auto-mode-alist '("\\.scss$" . css-mode))
(add-to-list 'auto-mode-alist '("\\.ts$" . typescript-mode))
(add-to-list 'auto-mode-alist '("\\.tsx$" . typescript-mode))
(add-to-list 'auto-mode-alist '(".*stumpwmrc$" . lisp-mode))
(add-to-list 'auto-mode-alist '("\\.gitignore$" . conf-mode))
(add-to-list 'auto-mode-alist '("crontab\\.[a-zA-Z0-9]+\\'" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.mdx$" . markdown-mode))

(add-hook 'after-change-major-mode-hook 'enable-linum-in-some-buffers)

