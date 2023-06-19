(require 'f)

(defun kec:initialize-packages ()
  (dolist (glob '("*.el" "*/init.el"))
    (dolist (file (f-glob (format "%s/config/pkg/%s" (directory-file-name kec:config-dir) glob)))
      (load file))))

(kec:initialize-packages)
    
