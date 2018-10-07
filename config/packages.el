(require 'package)

(custom-set-variables
 '(package-user-dir (concat kec:config-dir "elpa"))
 '(package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                      ;; ("marmalade" . "https://marmalade-repo.org/packages/")
                      ("melpa" . "https://melpa.org/packages/"))))

(unless package--initialized
  (package-initialize))

(unless (require 'quelpa nil t)
  (with-temp-buffer
    (url-insert-file-contents "https://raw.github.com/quelpa/quelpa/master/bootstrap.el")
    (eval-buffer)))

(unless package-archive-contents
  (message "before package-refresh-contens undo-tree %S" (package-installed-p 'undo-tree))
  (package-refresh-contents))

(defvar kec:packages nil)
(setq kec:packages
      (list
       'undo-tree
       'auto-complete
       'python-mode
       'web-mode
       'php-mode
       'bison-mode
       'goto-last-change
       'yaml-mode
       'dockerfile-mode
       'jdee
       'multi-term
       'yasnippet
       'ac-c-headers
       'rjsx-mode
       'go-mode
       'kotlin-mode
       'typescript-mode
       'default-text-scale
       'jedi
       ))

(defun kec:install-packages ()
  (message "elpa dir: %S" package-user-dir)
  (dolist (pkg kec:packages)
    (if (package-installed-p pkg)
        (message "Package %S is already installed" pkg)
      (message "Installing package %S..." pkg)
      (package-install pkg))))

(kec:install-packages)

(require 's)

(load "config/pkg/undo-tree")
(load "config/pkg/web")
(load "config/pkg/term")
(load "config/pkg/ac")
(load "config/pkg/c-ac")
(load "config/pkg/jdee")
(load "config/pkg/yasnippet")
(load "config/pkg/rjsx")
(load "config/pkg/python")
(load "config/pkg/ts")
(load "config/pkg/default-text-scale")
(load "config/pkg/jedi")
