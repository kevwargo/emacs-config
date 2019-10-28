(require 'package)

(package-initialize)

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
       'quelpa
       'kotlin-mode
       'typescript-mode
       'default-text-scale
       'jedi
       'indent-tools
       'magit
       'ac-slime
       'paredit
       'rainbow-delimiters
       'uuidgen
       'magit
       'neotree
       'go-guru
       'pylint
       ))

(defun kec:install-packages ()
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
(load "config/pkg/slime-ac")
(load "config/pkg/jdee")
(load "config/pkg/yasnippet")
(load "config/pkg/rjsx")
(load "config/pkg/python")
(load "config/pkg/ts")
(load "config/pkg/default-text-scale")
(load "config/pkg/jedi")
(load "config/pkg/indent-tools")
(load "config/pkg/yaml")
(load "config/pkg/go")
(load "config/pkg/neotree")
(load "config/pkg/magit")

(exec-path-from-shell-initialize)
