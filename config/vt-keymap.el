(setq vt-ctrl-pressed-mode-map (let ((map (make-sparse-keymap)))
                                      (define-key map [up] 'scroll-down-line)
                                      (define-key map [down] 'scroll-up-line)
                                      (define-key map [left] 'backward-word)
                                      (define-key map [right] 'forward-word)
                                      (define-key map (kbd "DEL") 'backward-delete-word)
                                      (define-key map (kbd "<deletechar>") 'delete-word)
                                      map))

(setq vt-quick-move-lines-mode-map (let ((map (make-sparse-keymap)))
                                     (define-key map [up] 'move-lines-up)
                                     (define-key map [down] 'move-lines-down)
                                     map))

(define-minor-mode vt-ctrl-pressed-mode
  "Defines"
  :init-value nil
  :lighter " CTRL"
  :keymap vt-ctrl-pressed-mode-map
  (setq vt-quick-move-lines-mode nil))

(define-minor-mode vt-quick-move-lines-mode
  "Maps [up] and [down] to `move-lines-up' and `move-lines-down'."
  :init-value nil
  :lighter " Move-Lines"
  :keymap vt-quick-move-lines-mode-map
  (setq vt-ctrl-pressed-mode nil))


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
