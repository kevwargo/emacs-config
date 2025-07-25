(defun c-mode-keymap-modify ()
  (mapc 'keymap-local-unset '("C-d" "C-c C-d" "C-c C-\\"))
  (keymap-local-set "(" 'self-insert-command)
  (keymap-local-set ")" 'self-insert-command)
  (keymap-local-set "C-{" 'embrace-selected-lines)
  (keymap-local-set "C-;" 'c-finalize-string))

(defun c-finalize-string ()
  (interactive)
  (end-of-line)
  (insert ";")
  (funcall (key-binding (kbd "RET"))))

(defun c-define-style ()
  (add-to-list 'c-style-alist
               '("kevwargo"
                 (c-basic-offset . 4)
                 (c-comment-only-line-offset 0 . 0)
                 (c-offsets-alist
                  (inline-open . 0)
                  (statement-block-intro . +)
                  (substatement-open . 0)
                  (substatement-label . 0)
                  (label . 0)
                  (case-label . 0)
                  (arglist-intro . 8)
                  (arglist-cont-nonempty . 8)
                  (statement-cont . +)))))

(defun c-mode-for-lex-yacc ()
  (when (and (buffer-file-name)
             (string-match-p ".*\\.l$\\|.*\\.y$" (buffer-file-name)))
    (dolist (k '(";" ":" "," "{" "}"))
      (keymap-local-set k 'self-insert-command))))

(add-hook 'c-mode-common-hook 'c-mode-for-lex-yacc)
(add-hook 'c-mode-common-hook 'c-mode-keymap-modify)
(add-hook 'c-initialization-hook 'c-define-style)
