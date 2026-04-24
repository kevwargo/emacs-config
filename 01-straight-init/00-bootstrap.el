(defvar bootstrap-version)

(let* ((straight-lockfile (expand-file-name "default.el"
                                            (concat user-emacs-directory
                                                    "straight/versions")))
       (target-lockfile (expand-file-name "straight-versions-default.el"
                                          (file-name-directory
                                           (directory-file-name
                                            (file-name-directory load-file-name)))))
       (exists (or (file-exists-p straight-lockfile)
                   ;; to account for broken symlinks
                   (file-symlink-p straight-lockfile))))
  (when (and exists
             (not (equal (file-symlink-p straight-lockfile) target-lockfile))
             (y-or-n-p
              (format
               "Lockfile %s is not a symlink to %s.\nRemove it and re-initialize? "
               straight-lockfile target-lockfile)))
    (delete-file straight-lockfile)
    (setq exists nil))
  (unless exists
    (message "Initializing default lockfile to %s" target-lockfile)
    (make-symbolic-link target-lockfile straight-lockfile)))

(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
