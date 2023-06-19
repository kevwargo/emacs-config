(require 'package)

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(defvar kec:packages nil)
(setq kec:packages
      (list
       'connection
       'default-text-scale
       'dockerfile-mode
       'elpy
       'go-mode
       'go-tag
       'gotest
       'goto-last-change
       'graphql-mode
       'indent-tools
       'lsp-java
       'lsp-mode
       'magit
       'markdown-mode
       'multi-term
       'neotree
       'paredit
       'powershell
       'prettier
       'pylint
       'python-black
       'python-isort
       'restclient
       'string-inflection
       'typescript-mode
       'undo-tree
       'uuidgen
       'yaml-mode
       'yasnippet
       ))

(defun kec:install-packages ()
  (dolist (pkg kec:packages)
    (if (package-installed-p pkg)
        (message "Package %S is already installed" pkg)
      (message "Installing package %S..." pkg)
      (package-install pkg))
    ;; (condition-case err
    ;;     (load (file-name-concat "config/pkg" (symbol-name pkg)) t)
    ;;   (file-missing nil))
    ))

(kec:install-packages)

(require 's)

(load "config/pkg/undo-tree")
(load "config/pkg/term")
(load "config/pkg/java")
(load "config/pkg/yasnippet")
(load "config/pkg/python")
(load "config/pkg/ts")
(load "config/pkg/default-text-scale")
(load "config/pkg/indent-tools")
(load "config/pkg/yaml")
(load "config/pkg/go")
(load "config/pkg/neotree")
(load "config/pkg/magit")
(load "config/pkg/prettier")
(load "config/pkg/restclient/restclient")
