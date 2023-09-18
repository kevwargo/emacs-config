(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["black" "#d55e00" "#009e73" "#f8ec59" "#0072b2" "#cc79a7" "#56b4e9" "white"])
 '(c-report-syntactic-errors t)
 '(case-fold-search t)
 '(case-replace t)
 '(comint-input-ignoredups t)
 '(company-minimum-prefix-length 1)
 '(company-selection-wrap-around t)
 '(custom-enabled-themes '(adwaita))
 '(custom-file (concat kec:config-dir "config/custom.el"))
 '(custom-safe-themes
   '("f0c94bf6a29c232300e46af50f46ce337e721eacca6d618e8654a263db5ecdbe" "08a89acffece58825e75479333109e01438650d27661b29212e6560070b156cf" "ae3a3bed17b28585ce84266893fa3a4ef0d7d721451c887df5ef3e24a9efef8c" "2642a1b7f53b9bb34c7f1e032d2098c852811ec2881eec2dc8cc07be004e45a0" "adf5275cc3264f0a938d97ded007c82913906fc6cd64458eaae6853f6be287ce" default))
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
   '(editorconfig yasnippet json-mode csv-mode restclient-jq f company yaml-mode uuidgen undo-tree typescript-mode string-inflection restclient python-isort python-black pylint prettier powershell paredit neotree multi-term nil magit lsp-java lsp-mode indent-tools graphql-mode goto-last-change gotest go-tag go-mode elpy dockerfile-mode default-text-scale connection))
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
 '(default ((t (:inherit nil :extend nil :stipple nil :background "#EDEDED" :foreground "#2E3436" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 135 :width normal :foundry "PfEd" :family "Ubuntu Mono"))))
 '(Info-quoted ((t (:inherit fixed-pitch-serif :family "Ubuntu Mono"))))
 '(diff-file-header ((t (:background "grey60"))))
 '(diff-header ((t (:background "grey45" :foreground "black"))))
 '(ediff-odd-diff-A ((t (:extend t :background "#eecccc" :distant-foreground "Black"))))
 '(font-lock-comment-face ((t (:foreground "gray50" :slant italic))))
 '(font-lock-function-name-fac ((t :foreground "DarkBlue")) t)
 '(font-lock-keyword-face ((t :bold t)))
 '(font-lock-string-face ((t (:foreground "blue violet"))))
 '(font-lock-variable-name-face ((t (:foreground "forest green" :weight bold))))
 '(go-test--ok-face ((t (:foreground "forest green"))))
 '(go-test--warning-face ((t (:foreground "goldenrod"))))
 '(lsp-face-highlight-read ((t (:inherit highlight :background "olive drab" :underline t))))
 '(magit-section-highlight ((t (:background "ghost white"))))
 '(markdown-code-face ((t (:inherit fixed-pitch :height 110))))
 '(py-builtins-face ((t :foreground "#0057ae")))
 '(py-class-name-face ((t :foreground "DarkOliveGreen")))
 '(py-decorators-face ((t :foreground "SaddleBrown")))
 '(py-exception-name-face ((t :bold t :foreground "DarkGreen")))
 '(py-import-from-face ((t (:foreground "magenta" :weight normal))))
 '(py-number-face ((t :foreground "#b08000")))
 '(py-overloaders-face ((t (:inherit font-lock-keyword-face :foreground "lime green"))))
 '(py-pseudo-keyword-face ((t :foreground "#006e28")))
 '(py-variable-name-face ((t (:foreground "medium orchid"))))
 '(sh-escaped-newline ((t :foreground "Magenta")))
 '(sh-heredoc ((t (:foreground "chocolate"))))
 '(term-color-blue ((t :foreground "DodgerBlue1")))
 '(term-color-green ((t :foreground "green3" :background "pale green")))
 '(trailing-whitespace ((t (:background "yellow green")))))
