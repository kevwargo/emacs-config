(defun kec:after-init ()
  (tramp-unload-tramp)
  (load "config/packages"))

(add-hook 'after-init-hook 'kec:after-init)
