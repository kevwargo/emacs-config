(define-minor-mode key-overrides-mode
  "A minor mode with some custom keybindings"
  :keymap (let ((m (make-sparse-keymap)))
            (keymap-set m "C-M-q" 'quoted-insert)
            (keymap-set m "C-x u" 'undo-tree-visualize)
            (keymap-set m "C-x SPC" 'mark-current-line)
            (keymap-set m "C-x C-n" 'neotree-toggle)
            (keymap-set m "C-c ." 'find-function)
            (keymap-set m "M-[" 'sexp-traverse-goto-start)
            (keymap-set m "M-]" 'sexp-traverse-goto-end)
            m)
  :group 'key-overrides
  :global t)

(key-overrides-mode 1)
