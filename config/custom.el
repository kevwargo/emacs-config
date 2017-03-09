(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ajc-tag-file (concat kec:config-dir "java-base.tag"))
 '(ansi-color-names-vector
   ["black" "#d55e00" "#009e73" "#f8ec59" "#0072b2" "#cc79a7" "#56b4e9" "white"])
 '(c-default-style (quote ((awk-mode . "awk") (other . "kevwargo"))))
 '(c-report-syntactic-errors t)
 '(case-replace nil)
 '(comint-input-ignoredups t)
 '(custom-file (concat kec:config-dir "config/custom.el"))
 '(font-use-system-font t)
 '(ido-auto-merge-delay-time 2)
 '(jde-compiler (quote ("javac")))
 '(jde-gen-k&r nil)
 '(jde-jdk-registry (quote (("1.8.0.92" . "/opt/oracle-jdk-bin-1.8.0.92"))))
 '(jde-mode-line-format
   (quote
    ("%e" mode-line-front-space mode-line-mule-info mode-line-client mode-line-modified mode-line-remote mode-line-frame-identification mode-line-buffer-identification "   " mode-line-position "  " mode-line-modes mode-line-misc-info mode-line-end-spaces)))
 '(jdee-server-dir "/mnt/develop/my/lisp/emacs/emacs-config/jdee-server")
 '(jit-lock-stealth-verbose t)
 '(package-user-dir (concat kec:config-dir "elpa"))
 '(require-final-newline t)
 '(safe-local-variable-values
   (quote
    ((Package ITERATE :use "COMMON-LISP" :colon-mode :external)
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
 '(tool-bar-mode nil)
 '(wisent-parse-max-stack-size 1500))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t :height 90)))
 '(font-lock-comment-face ((t :foreground "grey53" :italic t)))
 '(font-lock-function-name-fac ((t :foreground "DarkBlue")) t)
 '(font-lock-keyword-face ((t :bold t)))
 '(font-lock-string-face ((t (:foreground "orange red"))))
 '(font-lock-variable-name-face ((t (:foreground "lime green"))))
 '(py-builtins-face ((t :foreground "#0057ae")))
 '(py-class-name-face ((t :foreground "DarkOliveGreen")))
 '(py-decorators-face ((t :foreground "SaddleBrown")))
 '(py-exception-name-face ((t :bold t :foreground "DarkGreen")))
 '(py-number-face ((t :foreground "#b08000")))
 '(py-overloaders-face ((t (:inherit font-lock-keyword-face :foreground "DodgerBlue1"))))
 '(py-pseudo-keyword-face ((t :foreground "#006e28")))
 '(py-variable-name-face ((t :foreground "DarkMagenta")))
 '(region ((t :background "steelblue1")))
 '(sh-escaped-newline ((t :foreground "Magenta")))
 '(term-color-blue ((t :foreground "DodgerBlue1")))
 '(term-color-green ((t :foreground "green3" :background "pale green")))
 '(trailing-whitespace ((t (:background "yellow green"))))
 '(web-mode-doctype-face ((t (:foreground "dim gray" :bold t))))
 '(web-mode-html-attr-name-face ((t (:foreground "dark green"))))
 '(web-mode-html-tag-face ((t (:foreground "deep sky blue")))))
