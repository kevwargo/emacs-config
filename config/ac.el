(require 'auto-complete-config)

;; (add-to-list 'ac-dictionary-directories "~/.emacs.d/elpa/auto-complete-20131118.1433/dict")
(ac-config-default)
(setq ac-auto-start t)
(setq ac-ignore-case nil)
(ac-set-trigger-key "<C-tab>")
(push 'jde-mode ac-modes)
