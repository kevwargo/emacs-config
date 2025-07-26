;; Keybindings for window system

(keymap-global-set "C-<backspace>" 'backward-delete-word)
(keymap-global-set "C-<return>" 'newline-from-middle-of-line)
(keymap-global-set "M-DEL" 'backward-delete-word)
(keymap-global-set "C-<delete>" 'delete-word)
(keymap-global-set "C-SPC" 'smart-set-mark-command)
(keymap-global-set "C-<down>" 'scroll-up-line)
(keymap-global-set "C-<up>" 'scroll-down-line)
(keymap-global-set "C-S-z" 'undo-tree-redo)
(keymap-global-set "C-S-d" 'uncomment-lines)
(keymap-global-set "C-S-<up>" 'move-lines-up)
(keymap-global-set "C-S-<down>" 'move-lines-down)
(keymap-global-set "C-M-<up>" 'copy-lines-up)
(keymap-global-set "C-M-<down>" 'copy-lines-down)
(keymap-global-set "<backtab>" 'insert-tab-command)

(keymap-global-set "C-S-f" 'findgrep)

(keymap-global-set "M-S-SPC" 'mark-current-word)
(keymap-global-set "C-M-SPC" 'mark-current-sexp)

(keymap-global-set "M->" 'xref-find-definitions-other-window)

(keymap-global-set "C-n" 'make-frame)

(define-minor-mode x-keys
  "Some custom keybindings working only from X"
  :keymap (let ((m (make-sparse-keymap)))
            (keymap-set m "M-<left>" 'windmove-left)
            (keymap-set m "M-<right>" 'windmove-right)
            (keymap-set m "M-<up>" 'windmove-up)
            (keymap-set m "M-<down>" 'windmove-down)
            (keymap-set m "C-c C-u" 'browse-url-at-point)
            m)
  :group 'keybinding-x
  :global t)

(x-keys 1)
