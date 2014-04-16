(defun comint-rebind-keys ()
  (local-set-key [up] 'comint-previous-input)
  (local-set-key [down] 'comint-next-input)
  (local-set-key (kbd "C-<up>") 'previous-line)
  (local-set-key (kbd "C-<down>") 'next-line)
  (dolist (key '("l" "a" "s" "w" "d" "x"))
    (local-unset-key (kbd (concat "C-c C-" key)))))

(defun make-frame-set-parameters (frame)
  (cond ((window-system frame)
         (set-frame-parameter frame 'fullscreen 'maximized)
         (blink-cursor-mode 1))
        (t
         (set-frame-parameter frame 'menu-bar-lines 0))))

(defun c-mode-keymap-modify ()
  (dolist (key '("C-d" "C-c C-d" "C-c C-\\"))
    (local-unset-key (kbd key))))

(defun sh-mode-keymap-modify ()
  (define-key (current-local-map) (kbd "C-j") 'newline)
  (define-key (current-local-map) (kbd "RET") 'newline))

(defun term-mode-keymap-modify ()
  (dolist (key '("<M-left>" "<M-right>"))
    (define-key term-raw-map (kbd key) nil))
  (define-key term-raw-map (kbd "C-c SPC") 'term-line-mode)
  (define-key term-mode-map (kbd "C-c SPC") 'term-char-mode))

(defun c-define-style ()
  (setq c-style-alist (append c-style-alist
                              '(("kevwargo"
                                 (c-basic-offset . 4)
                                 (c-comment-only-line-offset 0 . 0)
                                 (c-offsets-alist
                                  (inline-open . 0)
                                  (statement-block-intro . +)
                                  (substatement-open . 0)
                                  (substatement-label . 0)
                                  (label . 0)
                                  (case-label . +)
                                  (statement-cont . +)))))))

(defun elisp-mode-hook ()
  (setq tab-width 8))

(defun enable-linum-mode ()
  (linum-mode 1))

(add-hook 'comint-mode-hook 'comint-rebind-keys)
(add-hook 'after-make-frame-functions 'make-frame-set-parameters)
(add-hook 'c-mode-common-hook 'c-mode-keymap-modify)
;; (add-hook 'c++-mode-hook 'c-mode-keymap-modify)
;; (add-hook 'jde-mode-hook 'c-mode-keymap-modify)
(add-hook 'term-mode-hook 'term-mode-keymap-modify)
(add-hook 'c-initialization-hook 'c-define-style)
(add-hook 'sh-mode-hook 'sh-mode-keymap-modify)
(add-hook 'emacs-lisp-mode-hook 'elisp-mode-hook)

;; (add-hook 'jde-mode-hook 'ajc-java-complete-mode)
;; (add-hook 'find-file-hook 'enable-linum-mode)

