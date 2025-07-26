(define-minor-mode vt-ctrl-pressed-mode
  "Defines"
  :init-value nil
  :lighter " CTRL"
  :keymap (let ((map (make-sparse-keymap)))
            (keymap-set map "<up>" 'scroll-down-line)
            (keymap-set map "<down>" 'scroll-up-line)
            (keymap-set map "<left>" 'backward-word)
            (keymap-set map "<right>" 'forward-word)
            (keymap-set map "DEL" 'backward-delete-word)
            (keymap-set map "<deletechar>" 'delete-word)
            map)
  (if vt-ctrl-pressed-mode
      (vt-quick-move-lines-mode 0)))

(define-minor-mode vt-quick-move-lines-mode
  "Maps [up] and [down] to `move-lines-up' and `move-lines-down'."
  :init-value nil
  :lighter " Move-Lines"
  :keymap (let ((map (make-sparse-keymap)))
            (keymap-set map "<up>" 'move-lines-up)
            (keymap-set map "<down>" 'move-lines-down)
            map)
  (if vt-quick-move-lines-mode
      (vt-ctrl-pressed-mode 0)))

;; Keybindings for terminal
(keymap-global-set "M-;" 'vt-ctrl-pressed-mode)
(keymap-global-set "M-'" 'vt-quick-move-lines-mode)
(keymap-global-set "C-@" 'smart-set-mark-command)
(keymap-global-set "C-c C-z" 'undo-tree-redo)
(keymap-global-set "C-c C-d" 'uncomment-lines)
(keymap-global-set "C-x <up>" 'copy-lines-up)
(keymap-global-set "C-x <down>" 'copy-lines-down)
(keymap-global-set "C-c <left>" 'windmove-left)
(keymap-global-set "C-c <right>" 'windmove-right)
(keymap-global-set "C-c <up>" 'windmove-up)
(keymap-global-set "C-c <down>" 'windmove-down)
