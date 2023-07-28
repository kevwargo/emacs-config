;; Keybindings for window system

(global-set-key (kbd "<C-backspace>") 'backward-delete-word)
(global-set-key (kbd "<C-return>") 'newline-from-middle-of-line)
(global-set-key (kbd "M-DEL") 'backward-delete-word)
(global-set-key (kbd "<C-delete>") 'delete-word)
(global-set-key (kbd "C-SPC") 'smart-set-mark-command)
(global-set-key (kbd "C-<down>") 'scroll-up-line)
(global-set-key (kbd "C-<up>") 'scroll-down-line)
(global-set-key (kbd "C-S-z") 'undo-tree-redo)
(global-set-key (kbd "C-S-d") 'uncomment-lines)
(global-set-key (kbd "C-S-<up>") 'move-lines-up)
(global-set-key (kbd "C-S-<down>") 'move-lines-down)
(global-set-key (kbd "C-M-<up>") 'copy-lines-up)
(global-set-key (kbd "C-M-<down>") 'copy-lines-down)
(global-set-key (kbd "s-<left>") 'backward-sexp)
(global-set-key (kbd "s-<right>") 'forward-sexp)
(global-set-key (kbd "<backtab>") 'insert-tab-command)
(global-set-key (kbd "s-z") 'keyboard-quit)

(global-set-key (kbd "C-S-f") 'findgrep)

(global-set-key (kbd "M-S-SPC") 'mark-current-word)
(global-set-key (kbd "C-M-SPC") 'mark-current-sexp)

(global-set-key (kbd "M->") 'xref-find-definitions-other-window)

(global-set-key (kbd "C-n") 'make-frame)

(define-minor-mode x-keys
  "Some custom keybindings working only from X"
  :keymap (let ((m (make-sparse-keymap)))
            (define-key m (kbd "M-<left>") 'windmove-left)
            (define-key m (kbd "M-<right>") 'windmove-right)
            (define-key m (kbd "M-<up>") 'windmove-up)
            (define-key m (kbd "M-<down>") 'windmove-down)
            (define-key m (kbd "C-c C-u") 'browse-url-at-point)
            m)
  :global t)

(x-keys 1)
