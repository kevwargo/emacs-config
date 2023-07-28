(defvar kec:packages nil)
(setq kec:packages
      '(
        company
        connection
        default-text-scale
        dockerfile-mode
        f
        go-mode
        go-tag
        gotest
        goto-last-change
        graphql-mode
        indent-tools
        json-mode
        lsp-java
        lsp-mode
        magit
        markdown-mode
        multi-term
        neotree
        paredit
        powershell
        prettier
        pylint
        python-black
        python-isort
        restclient
        s
        string-inflection
        typescript-mode
        undo-tree
        uuidgen
        yaml-mode
        yasnippet
        ))

(defun kec:install-packages ()
  (dolist (pkg kec:packages)
    (if (package-installed-p pkg)
        (message "Package %S is already installed" pkg)
      (message "Installing package %S..." pkg)
      (package-install pkg))))

(kec:install-packages)
