(setq inferior-lisp-program "sbcl")
(setq slime-lisp-implementations
      '((sbcl ("sbcl" "--noinform" "--no-linedit")))); "--core" "/home/vozhyk/dev/lisp/sbcl.core-for-slime"))
;; 	(clisp ("clisp"))
;; 	(abcl ("abcl"))
;; 	(cmucl ("lisp"))
;; 	(ccl ("ccl" "-K" "utf-8"))
;; 	(ecl ("ecl"))))
(setq slime-net-coding-system 'utf-8-unix)

;; (load (expand-file-name "~/quicklisp/slime-helper.el"))
(defun slime-repl-custom-keys-hook ()
  (local-set-key (kbd "C-c C-k") 'slime-repl-delete-current-input))

(add-hook 'slime-repl-mode-hook 'slime-repl-custom-keys-hook)
