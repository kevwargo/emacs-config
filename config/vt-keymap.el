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
  nil
  " CTRL"
  vt-ctrl-pressed-mode-map
  (setq vt-quick-move-lines-mode nil))

(define-minor-mode vt-quick-move-lines-mode
  "Maps [up] and [down] to `move-lines-up' and `move-lines-down'."
  nil
  " Move-Lines"
  vt-quick-move-lines-mode-map
  (setq vt-ctrl-pressed-mode nil))


;; Keybindings for terminal
(global-set-key (kbd "M-;") 'vt-ctrl-pressed-mode)
(global-set-key (kbd "M-'") 'vt-quick-move-lines-mode)
(global-set-key (kbd "C-@") 'smart-set-mark-command)
(global-set-key (kbd "C-c C-z") 'undo-tree-redo)
(global-set-key (kbd "C-c C-d") 'uncomment-lines)
(global-set-key (kbd "C-x <up>") 'copy-lines-up)
(global-set-key (kbd "C-x <down>") 'copy-lines-down)
(global-set-key (kbd "C-c <left>") 'windmove-left)
(global-set-key (kbd "C-c <right>") 'windmove-right)
(global-set-key (kbd "C-c <up>") 'windmove-up)
(global-set-key (kbd "C-c <down>") 'windmove-down)

