(define-minor-mode key-overrides-mode
  "A minor mode with some custom keybindings"
  :keymap (let ((m (make-sparse-keymap)))
            (define-key m (kbd "C-M-q") 'quoted-insert)
            (define-key m (kbd "C-x u") 'undo-tree-visualize)
            (define-key m (kbd "C-x SPC") 'mark-current-line)
            (define-key m (kbd "C-x C-n") 'neotree-toggle)
            (define-key m (kbd "C-c .") 'find-function)
            m)
  :global t)

(key-overrides-mode 1)
