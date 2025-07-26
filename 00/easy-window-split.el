(define-minor-mode easy-window-split
  "Split windows horizontally or vertically with C-= and C-| respectively"
  :keymap (let ((m (make-sparse-keymap)))
            (keymap-set m "C-=" 'split-window-vertically)
            (keymap-set m "C-|" 'split-window-horizontally)
            m)
  :group 'easy-window-split
  :global t)

(easy-window-split 1)
