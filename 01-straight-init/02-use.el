;; -*- lexical-binding: t -*-

(dolist (pkg '((advanced-pos-mode . "advanced-pos-mode.el")
               (findgrep . "findgrep.el")
               (ivy-xref . "ivy-xref.el")
               (kwinjs-repl . "kwinjs-repl.el")
               (restclient . "restclient")
               (restclient-aws . "restclient-aws")))
  (straight-register-package
   (list (car pkg)
         :host 'github
         :repo (format "kevwargo/%s" (cdr pkg))
         :protocol 'ssh)))

(dolist (pkg '(advanced-pos-mode
               company
               connection
               copilot
               csv-mode
               default-text-scale
               dockerfile-mode
               f
               go-mode
               go-tag
               gotest
               goto-last-change
               graphql-mode
               groovy-mode
               findgrep
               indent-tools
               ivy-xref
               json-mode
               json-navigator
               kotlin-mode
               kwinjs-repl
               lsp-java
               lsp-mode
               magit
               markdown-mode
               multi-term
               neotree
               powershell
               prettier
               pylint
               qml-mode
               reformatter
               restclient
               restclient-jq
               restclient-aws
               rjsx-mode
               s
               string-inflection
               sudo-edit
               undo-tree
               uuidgen
               yaml-mode
               yasnippet))
  (straight-use-package pkg))
