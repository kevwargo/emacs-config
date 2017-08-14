(defun make-frame-set-parameters (frame)
  (cond ((window-system frame)
         (set-frame-parameter frame 'fullscreen 'maximized)
         (load-theme 'deeper-blue)
         (blink-cursor-mode 1))
        (t
         (set-frame-parameter frame 'menu-bar-lines 0))))

(defun elisp-mode-hook ()
  (setq-local electric-pair-pairs (list '(?\" . ?\")))
  (setq tab-width 8))

(defun css-mode-hook-misc ()
  (local-set-key (kbd "C-{") 'embrace-selected-lines))

(add-hook 'after-make-frame-functions 'make-frame-set-parameters)
(add-hook 'emacs-lisp-mode-hook 'elisp-mode-hook)
(add-hook 'isearch-mode-hook
          (function
           (lambda ()
             (define-key isearch-mode-map "\C-h" 'isearch-mode-help)
             (define-key isearch-mode-map "\C-t" 'isearch-toggle-regexp)
             (define-key isearch-mode-map "\C-c" 'isearch-toggle-case-fold)
             (define-key isearch-mode-map "\C-j" 'isearch-edit-string))))
(add-hook 'css-mode-hook 'css-mode-hook-misc)

(remove-hook 'find-file-hook 'vc-find-file-hook)
