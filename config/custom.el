(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ajc-tag-file (concat kec:config-dir "java-base.tag"))
 '(c-default-style (quote ((java-mode . "java") (awk-mode . "awk") (other . "kevwargo"))))
 '(case-replace nil)
 '(comint-input-ignoredups t)
 '(custom-file (concat kec:config-dir "config/custom.el"))
 '(font-use-system-font t)
 '(jde-compiler (quote ("javac")))
 '(jde-gen-k&r nil)
 '(jde-jdk-registry (quote (("1.7.0.67" . "/opt/oracle-jdk-bin-1.7.0.67"))))
 '(jde-mode-line-format (quote ("%e" mode-line-front-space mode-line-mule-info mode-line-client mode-line-modified mode-line-remote mode-line-frame-identification mode-line-buffer-identification "   " mode-line-position "  " mode-line-modes mode-line-misc-info mode-line-end-spaces)))
 '(jit-lock-stealth-verbose t)
 '(package-user-dir (concat kec:config-dir "elpa"))
 '(require-final-newline t)
 '(safe-local-variable-values (quote ((Package ITERATE :use "COMMON-LISP" :colon-mode :external) (syntax . COMMON-LISP) (Syntax . Ansi-Common-Lisp) (Package . CL-USER) (Package . CL-FAD) (Syntax . COMMON-LISP) (Base . 10) (Syntax . ANSI-Common-Lisp))))
 '(tool-bar-mode nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t :height 90)))
 '(font-lock-comment-face ((t :foreground "grey53" :italic t)))
 '(font-lock-function-name-fac ((t :foreground "DarkBlue")) t)
 '(font-lock-keyword-face ((t :bold t)))
 '(font-lock-string-face ((t :foreground "#bf0303")))
 '(font-lock-variable-name-face ((t :foreground "DarkGreen")))
 '(py-builtins-face ((t :foreground "#0057ae")))
 '(py-class-name-face ((t :foreground "DarkOliveGreen")))
 '(py-decorators-face ((t :foreground "SaddleBrown")))
 '(py-exception-name-face ((t :bold t :foreground "DarkGreen")))
 '(py-number-face ((t :foreground "#b08000")))
 '(py-pseudo-keyword-face ((t :foreground "#006e28")))
 '(py-variable-name-face ((t :foreground "DarkMagenta")))
 '(region ((t :background "steelblue1")))
 '(sh-escaped-newline ((t :foreground "Magenta")))
 '(trailing-whitespace ((t (:background "yellow green")))))
