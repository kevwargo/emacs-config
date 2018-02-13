(defun kec:after-init ()
  (setq tramp-mode nil)
  (load "config/packages")
)

(add-hook 'after-init-hook 'kec:after-init)
