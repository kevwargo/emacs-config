(setq custom-file load-file-name)

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
 '(ediff-split-window-function 'split-window-horizontally)
 '(ediff-window-setup-function 'ediff-setup-windows-plain)
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
 '(py-split-window-on-execute t)
 '(py-split-windows-on-execute-function 'split-window-horizontally)
 '(py-underscore-word-syntax-p nil)
 '(require-final-newline t)
 '(tab-always-indent t)
 '(tool-bar-mode nil)
 '(vc-follow-symlinks t)
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
 '(sh-heredoc ((t (:foreground "chocolate")))))
