(load "config/common-keymap")
(load "config/vt-keymap")
(load "config/x-keymap")

(defun comint-mode-keymap-modify ()
  (local-set-key [up] 'comint-previous-input)
  (local-set-key [down] 'comint-next-input)
  (local-set-key (kbd "C-<up>") 'previous-line)
  (local-set-key (kbd "C-<down>") 'next-line)
  (dolist (key '("l" "a" "s" "w" "d" "x"))
    (local-unset-key (kbd (concat "C-c C-" key)))))

(defun sh-mode-keymap-modify ()
  (define-key (current-local-map) (kbd "C-j") 'newline)
  (define-key (current-local-map) (kbd "RET") 'newline))

(defun term-mode-keymap-modify ()
  (dolist (key '("<M-left>" "<M-right>"))
    (define-key term-raw-map (kbd key) nil))
  (define-key term-raw-map (kbd "C-c SPC") 'term-line-mode)
  (define-key term-mode-map (kbd "C-c SPC") 'term-char-mode))

(defun elisp-mode-keymap-modify ()
  (local-unset-key (kbd "C-M-q")))

(add-hook 'comint-mode-hook 'comint-mode-keymap-modify)
(add-hook 'term-mode-hook 'term-mode-keymap-modify)
(add-hook 'sh-mode-hook 'sh-mode-keymap-modify)
(add-hook 'emacs-lisp-mode-hook 'elisp-mode-keymap-modify)
