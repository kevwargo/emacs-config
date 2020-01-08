(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ac-candidate-limit 2000)
 '(ajc-tag-file (concat kec:config-dir "java-base.tag"))
 '(android-mode-sdk-dir "/mnt/develop/sdk/android-sdk/android-sdk-linux")
 '(ansi-color-names-vector
   ["black" "#d55e00" "#009e73" "#f8ec59" "#0072b2" "#cc79a7" "#56b4e9" "white"])
 '(c-default-style (quote ((awk-mode . "awk") (other . "kevwargo"))))
 '(c-report-syntactic-errors t)
 '(case-fold-search t)
 '(case-replace t)
 '(comint-input-ignoredups t)
 '(custom-file (concat kec:config-dir "config/custom.el"))
 '(custom-safe-themes
   (quote
    ("08a89acffece58825e75479333109e01438650d27661b29212e6560070b156cf" "ae3a3bed17b28585ce84266893fa3a4ef0d7d721451c887df5ef3e24a9efef8c" "2642a1b7f53b9bb34c7f1e032d2098c852811ec2881eec2dc8cc07be004e45a0" "adf5275cc3264f0a938d97ded007c82913906fc6cd64458eaae6853f6be287ce" default)))
 '(eclim-eclipse-dirs (quote ("/mnt/data/eclipse/image/eclipse")))
 '(enable-local-variables :all)
 '(font-use-system-font t)
 '(go-guess-gopath-functions
   (quote
    (go-dep-gopath go-godep-gopath go-wgo-gopath go-gb-gopath go-plain-gopath)))
 '(gofmt-command "goimports")
 '(graphql-indent-level 2)
 '(grep-save-buffers nil)
 '(ido-auto-merge-delay-time 2)
 '(jde-compiler (quote ("javac")))
 '(jde-gen-k&r nil)
 '(jde-jdk-registry (quote (("1.8.0.92" . "/opt/oracle-jdk-bin-1.8.0.92"))))
 '(jde-mode-line-format
   (quote
    ("%e" mode-line-front-space mode-line-mule-info mode-line-client mode-line-modified mode-line-remote mode-line-frame-identification mode-line-buffer-identification "   " mode-line-position "  " mode-line-modes mode-line-misc-info mode-line-end-spaces)))
 '(jdee-ant-enable-find t)
 '(jdee-ant-read-target t)
 '(jdee-flycheck-enable-p nil)
 '(jdee-maven-disabled-p t)
 '(jdee-mode-line-format
   (quote
    ("-" mode-line-mule-info mode-line-modified mode-line-frame-identification mode-line-buffer-identification "   " mode-line-position
     (jdee-which-method-mode
      ("--" jdee-which-method-format "--"))
     mode-line-modes mode-line-misc-info mode-line-end-spaces)))
 '(jdee-server-dir (concat kec:config-dir "jdee-server"))
 '(jedi:environment-root "python3")
 '(jedi:environment-virtualenv
   (quote
    ("virtualenv" "--system-site-packages" "--python=python3")))
 '(jit-lock-stealth-verbose t)
 '(js2-strict-missing-semi-warning nil)
 '(markdown-command-needs-filename t)
 '(package-archives
   (quote
    (("gnu" . "https://elpa.gnu.org/packages/")
     ("melpa" . "https://melpa.org/packages/"))))
 '(package-selected-packages
   (quote
    (magit-popup magit-tbdiff graphql-mode gotest pylint ido-completing-read+ evil jinja2-mode jtags eclim lsp-java powershell python-mode flycheck-mypy csharp-mode magit-find-file magit-imerge malinka green-phosphor-theme green-screen-theme atom-dark-theme atom-one-dark-theme markdown-mode markdown-preview-mode flymd java-snippets go-guru neotree exec-path-from-shell magit uuidgen json-mode ac-slime paredit rainbow-delimiters slime jedi indent-tools default-text-scale typescript-mode kotlin-mode go-mode quelpa ac-c-headers yasnippet rjsx-mode multi-term jdee dockerfile-mode yaml-mode goto-last-change bison-mode php-mode web-mode auto-complete undo-tree)))
 '(package-user-dir (concat kec:config-dir "elpa"))
 '(py-split-window-on-execute t)
 '(py-split-windows-on-execute-function (quote split-window-horizontally))
 '(py-underscore-word-syntax-p nil)
 '(require-final-newline t)
 '(safe-local-variable-values
   (quote
    ((eval setq cc-search-directories
           (append
            (list
             (concat
              (locate-dominating-file
               (buffer-file-name)
               ".dir-locals.el")
              "/include"))
            cc-search-directories))
     (eval setq cc-search-directories
           (append
            (list
             (file-name-directory load-file-name))
            cc-search-directories))
     (cc-search-directories append
                            (list
                             (file-name-directory load-file-name))
                            cc-search-directories)
     (kec:c-func lambda nil load-file-name)
     (kec:c-func lambda nil
                 (load-file-name))
     (kec:c-var . "Lepecbeke")
     (css-indent-offset . 4)
     (css-indent-offset . 2)
     (Package ITERATE :use "COMMON-LISP" :colon-mode :external)
     (syntax . COMMON-LISP)
     (Syntax . Ansi-Common-Lisp)
     (Package . CL-USER)
     (Package . CL-FAD)
     (Syntax . COMMON-LISP)
     (Base . 10)
     (Syntax . ANSI-Common-Lisp))))
 '(send-mail-function (quote smtpmail-send-it))
 '(smtpmail-smtp-server "smtp.googlemail.com")
 '(smtpmail-smtp-service 25)
 '(tab-always-indent t)
 '(tool-bar-mode nil)
 '(web-mode-enable-auto-indentation nil)
 '(wisent-parse-max-stack-size 1500))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t :height 120)))
 '(diff-file-header ((t (:background "grey60"))))
 '(diff-header ((t (:background "grey45" :foreground "black"))))
 '(font-lock-comment-face ((t (:foreground "gray50" :slant italic))))
 '(font-lock-function-name-fac ((t :foreground "DarkBlue")) t)
 '(font-lock-keyword-face ((t :bold t)))
 '(font-lock-string-face ((t (:foreground "blue violet"))))
 '(font-lock-variable-name-face ((t (:foreground "lime green"))))
 '(py-builtins-face ((t :foreground "#0057ae")))
 '(py-class-name-face ((t :foreground "DarkOliveGreen")))
 '(py-decorators-face ((t :foreground "SaddleBrown")))
 '(py-exception-name-face ((t :bold t :foreground "DarkGreen")))
 '(py-import-from-face ((t (:foreground "magenta" :weight normal))))
 '(py-number-face ((t :foreground "#b08000")))
 '(py-overloaders-face ((t (:inherit font-lock-keyword-face :foreground "lime green"))))
 '(py-pseudo-keyword-face ((t :foreground "#006e28")))
 '(py-variable-name-face ((t (:foreground "light pink"))))
 '(sh-escaped-newline ((t :foreground "Magenta")))
 '(term-color-blue ((t :foreground "DodgerBlue1")))
 '(term-color-green ((t :foreground "green3" :background "pale green")))
 '(trailing-whitespace ((t (:background "yellow green"))))
 '(web-mode-doctype-face ((t (:foreground "dim gray" :bold t))))
 '(web-mode-html-attr-name-face ((t (:foreground "dark green"))))
 '(web-mode-html-tag-face ((t (:foreground "deep sky blue")))))
