(defun c-mode-keymap-modify ()
  (dolist (key '("C-d" "C-c C-d" "C-c C-\\"))
    (eval `(local-unset-key (kbd ,key))))
  (local-unset-key (kbd "C-M-q"))
  (local-set-key (kbd "C-;") 'c-finalize-string))

(defun c-finalize-string ()
  (interactive)
  (end-of-line)
  (insert ";")
  (funcall (key-binding (kbd "RET"))))

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

(add-hook 'c-mode-common-hook 'c-mode-keymap-modify)
(add-hook 'c-initialization-hook 'c-define-style)
