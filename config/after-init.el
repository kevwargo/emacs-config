(defun kec:after-init ()
  ;; (load "config/eclim")
  (tramp-unload-tramp)
  (load "config/ac"))

(add-hook 'after-init-hook 'kec:after-init)
