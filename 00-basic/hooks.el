(defun make-frame-set-parameters (frame)
  (cond ((window-system frame)
         (set-frame-parameter frame 'fullscreen 'maximized)
         (load-theme 'adwaita)
         (blink-cursor-mode 1))
        (t
         (set-frame-parameter frame 'menu-bar-lines 0))))

(defun elisp-mode-hook ()
  (setq-local electric-pair-pairs (list '(?\" . ?\")))
  (keymap-local-set "C-M-z" 'eval-defun)
  (setq tab-width 8))

(defun lisp-mode-customize ()
  (keymap-local-set "C-M-z" 'slime-eval-defun))

(defun css-mode-hook-misc ()
  (keymap-local-set "C-{" 'embrace-selected-lines))

(add-hook 'after-make-frame-functions 'make-frame-set-parameters)
(add-hook 'emacs-lisp-mode-hook 'elisp-mode-hook)
(add-hook 'lisp-mode-hook 'lisp-mode-customize)
(add-hook 'css-mode-hook 'css-mode-hook-misc)

(remove-hook 'find-file-hook 'vc-find-file-hook)

(keymap-set isearch-mode-map "C-h" 'isearch-mode-help)
(keymap-set isearch-mode-map "C-t" 'isearch-toggle-regexp)
(keymap-set isearch-mode-map "C-c" 'isearch-toggle-case-fold)
(keymap-set isearch-mode-map "C-j" 'isearch-edit-string)
