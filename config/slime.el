(load (expand-file-name "~/quicklisp/slime-helper.el"))
(setq inferior-lisp-program "sbcl")
(setq slime-lisp-implementations
      '((sbcl ("sbcl" "--noinform" "--no-linedit"))))
(setq slime-net-coding-system 'utf-8-unix)

(defun slime-repl-custom-keys-hook ()
  (local-set-key (kbd "C-c C-k") 'slime-repl-delete-current-input))

(defun slime-set-package-to-stumpwm ()
  (if (= (slime-connection-port (slime-current-connection)) 44005)
      (slime-repl-set-package ":stumpwm")))

(defun slime-stumpwm ()
  (interactive)
  (slime-connect "localhost" 44005))

(add-hook 'slime-repl-mode-hook 'slime-repl-custom-keys-hook)
(add-hook 'slime-repl-mode-hook 'slime-set-package-to-stumpwm)
