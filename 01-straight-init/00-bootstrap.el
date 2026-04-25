(defvar bootstrap-version)

(defun setup-straight-lockfile ()
  (let* ((straight-lockfile-dir (concat user-emacs-directory "straight/versions"))
         (straight-lockfile (expand-file-name "default.el" straight-lockfile-dir))
         (link-target (ignore-errors
                        (expand-file-name (file-symlink-p straight-lockfile)
                                          straight-lockfile-dir)))
         (repo-lockfile (expand-file-name "straight-versions-default.el"
                                          (file-name-directory
                                           (directory-file-name
                                            (file-name-directory load-true-file-name)))))
         (exists-p (file-exists-p straight-lockfile))
         (broken-link-p (and (not exists-p) (file-symlink-p straight-lockfile))))

    (message "Checking straight.el lockfile: %s %s. Repo lockfile: %s"
             straight-lockfile
             (cond
              (broken-link-p "is a broken symlink")
              (link-target (format "points to %s" link-target))
              (t "is not a symlink"))
             repo-lockfile)

    (if broken-link-p (delete-file straight-lockfile))

    (when (and exists-p
               (not (equal link-target repo-lockfile))
               (y-or-n-p
                (format
                 "Lockfile %s is not a symlink to %s.\nRemove it and re-initialize? "
                 straight-lockfile repo-lockfile)))
      (delete-file straight-lockfile)
      (setq exists-p nil))
    (unless exists-p
      (message "Initializing default lockfile to %s" repo-lockfile)
      (make-symbolic-link
       (file-relative-name repo-lockfile straight-lockfile-dir)
       straight-lockfile))))

(defun bootstrap-straight ()
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
    (load bootstrap-file nil 'nomessage)))

(setup-straight-lockfile)
(bootstrap-straight)
