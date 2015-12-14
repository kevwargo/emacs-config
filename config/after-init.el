(defun kec:after-init ()
  (load "config/eclim")
  (load "config/ac"))

(add-hook 'after-init-hook 'kec:after-init)
