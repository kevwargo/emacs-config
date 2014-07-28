(load "config/common-keymap")
(load "config/vt-keymap")
(load "config/x-keymap")

(defun sh-mode-keymap-modify ()
  (define-key (current-local-map) (kbd "C-j") 'newline)
  (define-key (current-local-map) (kbd "RET") 'newline)
  (local-unset-key (kbd "C-c C-x")))

(defun term-mode-keymap-modify ()
  (dolist (key '("<M-left>" "<M-right>"))
    (eval `(define-key term-raw-map (kbd ,key) nil)))
  (define-key term-raw-map (kbd "C-r") 'term-send-raw)
  (define-key term-raw-map (kbd "C-s") 'term-send-raw)
  (define-key term-raw-map (kbd "C-c SPC") 'term-line-mode)
  (define-key term-mode-map (kbd "C-c SPC") 'term-char-mode))

(defun elisp-mode-keymap-modify ()
  (local-unset-key (kbd "C-M-q")))

(defun conf-mode-keymap-modify ()
  (local-unset-key (kbd "C-c C-x")))

(add-hook 'term-mode-hook 'term-mode-keymap-modify)
(add-hook 'sh-mode-hook 'sh-mode-keymap-modify)
(add-hook 'emacs-lisp-mode-hook 'elisp-mode-keymap-modify)
(add-hook 'conf-mode-hook 'conf-mode-keymap-modify)
