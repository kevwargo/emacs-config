(require 'package)

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(defvar kec:packages nil)
(setq kec:packages
      (list
       'company
       'connection
       'default-text-scale
       'dockerfile-mode
       'elpy
       'f
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
       's
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
      (package-install pkg))))

(kec:install-packages)

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
