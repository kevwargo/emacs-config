(global-subword-mode)
(global-display-line-numbers-mode)

(column-number-mode t)
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
(add-to-list 'auto-mode-alist '("\\.gitignore$" . conf-mode))
(add-to-list 'auto-mode-alist '("crontab\\.[a-zA-Z0-9]+\\'" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.mdx$" . markdown-mode))
