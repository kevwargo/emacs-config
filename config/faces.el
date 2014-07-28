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

(put 'minibuffer-prompt 'face-defface-spec
     (cons '(((type tty))
             :foreground "green")
           (get 'minibuffer-prompt 'face-defface-spec)))
