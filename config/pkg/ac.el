(require 'auto-complete-config)

(load "config/go-autocomplete.el")

(ac-config-default)
(setq ac-auto-start t)
(setq ac-ignore-case nil)

(push 'asm-mode ac-modes)
(push 'PDDL-mode ac-modes)
(push 'web-mode ac-modes)
(push 'makefile-gmake-mode ac-modes)
(push 'typescript-mode ac-modes)
(push 'yaml-mode ac-modes)
(push 'rjsx-mode ac-modes)
(push 'graphql-mode ac-modes)

(define-key ac-mode-map (kbd "C-c TAB") 'auto-complete)
