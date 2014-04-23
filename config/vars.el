(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)


(setq auto-mode-alist (append auto-mode-alist '(("/etc/cdnet" . conf-mode)
                                                ("\\.e\\(build\\|class\\)" . sh-mode))))

(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)
(setq tab-always-indent 'complete)

(setq scroll-step 1)
(setq mouse-wheel-scroll-amount '(3 ((shift) . 3)))
(setq mouse-wheel-progressive-speed nil)

(set-language-environment "utf-8")

(put 'narrow-to-region 'disabled nil)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ajc-tag-file (concat kec:config-dir "java-base.tag"))
 '(c-default-style (quote ((java-mode . "java") (awk-mode . "awk") (other . "kevwargo"))))
 '(comint-input-ignoredups t)
 '(font-use-system-font t)
 '(jde-compiler (quote ("javac")))
 '(jde-gen-k&r nil)
 '(jde-jdk-registry (quote (("1.7.0.51" . "/opt/oracle-jdk-bin-1.7.0.51"))))
 '(jde-mode-line-format (quote ("%e"
                                mode-line-front-space
                                mode-line-mule-info
                                mode-line-client
                                mode-line-modified
                                mode-line-remote
                                mode-line-frame-identification
                                mode-line-buffer-identification
                                "   "
                                mode-line-position
                                "  "
                                mode-line-modes
                                mode-line-misc-info
                                mode-line-end-spaces)))
 '(safe-local-variable-values
   (quote ((Package ITERATE :use "COMMON-LISP" :colon-mode :external)
           (syntax . COMMON-LISP)
           (Syntax . Ansi-Common-Lisp)
           (Package . CL-USER)
           (Package . CL-FAD)
           (Syntax . COMMON-LISP)
           (Base . 10)
           (Syntax . ANSI-Common-Lisp))))
 '(term-bind-key-alist (quote (("C-c C-c" . term-interrupt-subjob)
                               ("C-p" . previous-line)
                               ("C-n" . next-line)
                               ("C-s" . isearch-forward)
                               ("C-r" . isearch-backward)
                               ("C-m" . term-send-raw)
                               ("C-_" . term-send-raw)
                               ("M-f" . term-send-forward-word)
                               ("M-<right>" . term-send-forward-word)
                               ("C-<right>" . term-send-forward-word)
                               ("M-b" . term-send-backward-word)
                               ("M-<left>" . term-send-backward-word)
                               ("C-<left>" . term-send-backward-word)
                               ("M-o" . term-send-backspace)
                               ("M-p" . term-send-up)
                               ("M-n" . term-send-down)
                               ("M-M" . term-send-forward-kill-word)
                               ("M-d" . term-send-forward-kill-word)
                               ("M-DEL" . term-send-forward-kill-word)
                               ("C-DEL" . term-send-forward-kill-word)
                               ("<C-delete>" . term-send-forward-kill-word)
                               ("M-N" . term-send-backward-kill-word)
                               ("M-<backspace>" . term-send-backward-kill-word)
                               ("C-<backspace>" . term-send-backward-kill-word)
                               ("M-r" . term-send-reverse-search-history)
                               ("M-," . term-send-input)
                               ("M-." . comint-dynamic-complete))))
 '(tool-bar-mode nil)
 '(package-user-dir (concat kec:config-dir "elpa")))

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


