(defvar sha1-sum-regex
  "/[[:xdigit:]]\\{40\\}-[^/]+$")  

(defvar backup-directory-sha1
  (concat user-emacs-directory
          "backups"))

(defvar auto-save-directory-sha1
  (concat user-emacs-directory
          "autosave"))

(defun make-backup-file-name-sha1 (file)
  (when file
    (concat backup-directory-sha1
            "/"
            (sha1 (file-name-directory file))
            "-"
            (file-name-nondirectory file))))

(defun backup-file-name-p (file)
  (string-match-p (concat "^"
                          backup-directory-sha1
                          sha1-sum-regex)
                  file))

(defun make-auto-save-file-name ()
  (when-let ((file (buffer-file-name)))
    (concat auto-save-directory-sha1
            "/"
            (sha1 (file-name-directory file))
            "-"
            (file-name-nondirectory file))))

(defun auto-save-file-name-p (filename)
  (string-match-p (concat "^"
                          auto-save-directory-sha1
                          sha1-sum-regex)
                  filename))


(setq make-backup-file-name-function 'make-backup-file-name-sha1)

(make-directory backup-directory-sha1 t)
(make-directory auto-save-directory-sha1 t)
