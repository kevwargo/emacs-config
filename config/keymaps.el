(load "config/common-keymap")
(load "config/vt-keymap")
(load "config/x-keymap")

(defun sh-mode-keymap-modify ()
  (define-key (current-local-map) (kbd "C-j") 'newline)
  (define-key (current-local-map) (kbd "RET") 'newline)
  (local-unset-key (kbd "C-c C-x")))

(defun elisp-mode-keymap-modify ()
  (local-unset-key (kbd "C-M-q"))
  (local-set-key (kbd "C-c RET") 'eval-print-last-sexp)
  (local-set-key (kbd "C-(") (lambda () (interactive) (insert ?\())))

(defun lisp-mode-keymap-modify ()
  (local-set-key (kbd "C-(") (lambda () (interactive) (insert ?\())))

(defun conf-mode-keymap-modify ()
  (local-unset-key (kbd "C-c C-x")))

(defun html-mode-keymap-modify ()
  (dolist (key '("1" "2" "3" "4"))
    (eval `(local-unset-key (kbd ,(concat "C-c " key))))))

(add-hook 'sh-mode-hook 'sh-mode-keymap-modify)
(add-hook 'emacs-lisp-mode-hook 'elisp-mode-keymap-modify)
(add-hook 'lisp-mode-hook 'lisp-mode-keymap-modify)
(add-hook 'conf-mode-hook 'conf-mode-keymap-modify)
(add-hook 'html-mode-hook 'html-mode-keymap-modify)
