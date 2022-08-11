(defun kec:after-init ()
  (setq tramp-mode nil)
  (load "config/packages")
  (load "utils/findgrep")
)

(add-hook 'after-init-hook 'kec:after-init)
