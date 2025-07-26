(defvar kec-config-dir (file-name-directory (file-truename load-file-name))
  "Directory containing the main config files.")

(defun load-kec-directory (dir)
  (setq dir (expand-file-name dir kec-config-dir))
  (dolist (item (directory-files dir nil "^[a-zA-Z0-9_-]+\\(\\.elc?\\)?$"))
    (setq item (expand-file-name item dir))
    (if (file-directory-p item)
        (load (expand-file-name (file-name-base item) item) t)
      (load item))))

(load-kec-directory "00-basic")
(load-kec-directory "01-straight-init")
(load-kec-directory "02-pkg-dependent")
