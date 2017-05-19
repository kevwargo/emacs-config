(require 'package)

(custom-set-variables
 '(package-user-dir (concat kec:config-dir "elpa"))
 '(package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                      ("marmalade" . "https://marmalade-repo.org/packages/")
                      ("melpa" . "https://melpa.org/packages/"))))

(unless package--initialized
  (package-initialize))
(unless package-archive-contents
  (package-refresh-contents))

(defvar kec:packages nil)
(setq kec:packages
      (list
       'undo-tree
       'auto-complete
       'python-mode
       'web-mode
       'php-mode
       'multi-term))

(defun kec:install-packages ()
  (dolist (pkg kec:packages)
    (unless (package-installed-p pkg)
      (package-install pkg))))

(kec:install-packages)

(load "config/pkg/undo-tree")
(load "config/pkg/web")
(load "config/pkg/term")
(load "config/pkg/ac")
(load "config/pkg/python")
