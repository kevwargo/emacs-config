(defun make-frame-set-parameters (frame)
  (cond ((window-system frame)
         (set-frame-parameter frame 'fullscreen 'maximized)
         (blink-cursor-mode 1))
        (t
         (set-frame-parameter frame 'menu-bar-lines 0))))

(defun elisp-mode-hook ()
  (setq tab-width 8))

(defun enable-linum-mode ()
  (linum-mode 1))

(add-hook 'after-make-frame-functions 'make-frame-set-parameters)
(add-hook 'emacs-lisp-mode-hook 'elisp-mode-hook)
;; (add-hook 'find-file-hook 'enable-linum-mode)
