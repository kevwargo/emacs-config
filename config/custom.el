(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-save-default nil)
 '(c-report-syntactic-errors t)
 '(case-fold-search t)
 '(case-replace t)
 '(comint-input-ignoredups t)
 '(company-minimum-prefix-length 1)
 '(company-selection-wrap-around t)
 '(create-lockfiles nil)
 '(custom-enabled-themes '(whiteboard deeper-blue))
 '(custom-file (concat kec:config-dir "config/custom.el"))
 '(ediff-split-window-function 'split-window-horizontally)
 '(enable-local-variables :all)
 '(font-use-system-font t)
 '(go-gen-test-enable-subtests nil)
 '(go-guess-gopath-functions '(go-plain-gopath))
 '(gofmt-command "goimports")
 '(graphql-indent-level 2)
 '(grep-save-buffers nil)
 '(ido-auto-merge-delay-time 2)
 '(jedi:environment-root "python3")
 '(jedi:environment-virtualenv '("virtualenv" "--system-site-packages" "--python=python3"))
 '(jedi:tooltip-method nil)
 '(jedi:use-shortcuts t)
 '(jit-lock-stealth-verbose t)
 '(js2-strict-missing-semi-warning nil)
 '(lsp-enable-file-watchers nil)
 '(magit-refresh-verbose nil)
 '(markdown-command-needs-filename t)
 '(org-support-shift-select t)
 '(package-selected-packages
   '(kwin kotlin-mode groovy-mode cmake-mode qml-mode markdown-mode project restclient s flymake-markdownlint grip-mode rjsx-mode editorconfig yasnippet json-mode csv-mode restclient-jq f company yaml-mode uuidgen undo-tree typescript-mode string-inflection python-isort python-black pylint prettier powershell paredit neotree multi-term magit lsp-java lsp-mode indent-tools graphql-mode goto-last-change gotest go-tag go-mode elpy dockerfile-mode default-text-scale connection))
 '(py-split-window-on-execute t)
 '(py-split-windows-on-execute-function 'split-window-horizontally)
 '(py-underscore-word-syntax-p nil)
 '(require-final-newline t)
 '(tab-always-indent t)
 '(tool-bar-mode nil)
 '(warning-suppress-log-types '((comp))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Ubuntu Mono" :height 135))))
 '(Info-quoted ((t (:inherit fixed-pitch-serif :family "Ubuntu Mono"))))
 '(font-lock-comment-face ((t (:foreground "gray50" :slant italic))))
 '(font-lock-function-name-fac ((t :foreground "DarkBlue")) t)
 '(font-lock-keyword-face ((t :bold t)))
 '(font-lock-string-face ((t (:foreground "blue violet"))))
 '(font-lock-variable-name-face ((t (:foreground "forest green" :weight bold))))
 '(go-test--ok-face ((t (:foreground "forest green"))))
 '(go-test--warning-face ((t (:foreground "goldenrod"))))
 '(sh-escaped-newline ((t :foreground "Magenta")))
 '(sh-heredoc ((t (:foreground "chocolate"))))
 )
