;; -*- lexical-binding: t; -*-

(defvar kec-config-dir (file-name-directory (file-truename load-file-name))
  "Directory containing the main config files.")

(defun load-kec-directory (dir)
  (setq dir (expand-file-name dir kec-config-dir))
  (dolist (item (directory-files dir t "^[a-zA-Z0-9_-]+\\(\\.elc?\\)?$"))
    (load item)))

(let ((ts (current-time)))
  (load-kec-directory "00-basic")
  (load-kec-directory "01-straight-init")
  (load-kec-directory "02-pkg-dependent")
  (message "Total init load time: %fs" (float-time (time-since ts))))
