(load "server")
(unless (server-running-p)
  (server-start))

(setq config-dir
      (file-name-directory load-file-name))

(dolist (path '(""  
                "kiwanami/emacs-window-manager/"
                "elpa/multi-term-20110326.608/"
                "elpa/auto-complete-20131128.233"
                "elpa/yasnippet-20131031.628"
                "elpa/auto-complete-clang-20120612.2224"
                "elpa/popup-20130708.2245"
                "auto-java-complete"))
  (add-to-list 'load-path (concat config-dir path)))

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)

(require 'show-point-mode)

;(require 'auto-complete-clang)

(column-number-mode t)
(show-point-mode t)
(global-undo-tree-mode t)
;; (global-linum-mode t)
(delete-selection-mode t)
(scroll-bar-mode 0)

;; This need to be set BEFORE enabling winner
(setq winner-dont-bind-my-keys t)
(winner-mode t)

(load "goto-last-change")
(load "custom/vars")
(load "custom/ac")
(load "custom/general-utils")
(load "custom/hooks")
(load "custom/keymaps")
(load "custom/lisp-utils")
(load "custom/python")
(load "custom/windmove-extra")
(load "custom/window-expand")
;; (load "custom/xsel")
(load "custom/jde-mode-extra")
(load "custom/slime")


(show-paren-mode 1)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ajc-tag-file "~/.emacs.d/java-base.tag")
 '(c-default-style (quote ((java-mode . "java") (awk-mode . "awk") (other . "kevwargo"))))
 '(comint-input-ignoredups t)
 '(font-use-system-font t)
 '(jde-compiler (quote ("javac")))
 '(jde-gen-k&r nil)
 '(jde-jdk-registry (quote (("1.7.0.45" . "/opt/oracle-jdk-bin-1.7.0.45"))))
 '(jde-mode-line-format (quote ("%e" mode-line-front-space mode-line-mule-info mode-line-client mode-line-modified mode-line-remote mode-line-frame-identification mode-line-buffer-identification "   " mode-line-position "  " mode-line-modes mode-line-misc-info mode-line-end-spaces)))
 '(safe-local-variable-values (quote ((Package ITERATE :use "COMMON-LISP" :colon-mode :external) (syntax . COMMON-LISP) (Syntax . Ansi-Common-Lisp) (Package . CL-USER) (Package . CL-FAD) (Syntax . COMMON-LISP) (Base . 10) (Syntax . ANSI-Common-Lisp))))
 '(term-bind-key-alist (quote (("C-c C-c" . term-interrupt-subjob) ("C-p" . previous-line) ("C-n" . next-line) ("C-s" . isearch-forward) ("C-r" . isearch-backward) ("C-m" . term-send-raw) ("C-_" . term-send-raw) ("M-f" . term-send-forward-word) ("M-<right>" . term-send-forward-word) ("C-<right>" . term-send-forward-word) ("M-b" . term-send-backward-word) ("M-<left>" . term-send-backward-word) ("C-<left>" . term-send-backward-word) ("M-o" . term-send-backspace) ("M-p" . term-send-up) ("M-n" . term-send-down) ("M-M" . term-send-forward-kill-word) ("M-d" . term-send-forward-kill-word) ("M-DEL" . term-send-forward-kill-word) ("C-DEL" . term-send-forward-kill-word) ("M-N" . term-send-backward-kill-word) ("M-<backspace>" . term-send-backward-kill-word) ("C-<backspace>" . term-send-backward-kill-word) ("M-r" . term-send-reverse-search-history) ("M-," . term-send-input) ("M-." . comint-dynamic-complete))))
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

(put 'narrow-to-region 'disabled nil)
