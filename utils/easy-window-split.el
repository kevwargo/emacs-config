(define-minor-mode easy-window-split
  "Split windows horizontally or vertically with C-_ and C-| respectively"
  :keymap (let ((m (make-sparse-keymap)))
            (define-key m (kbd "C-_") 'split-window-vertically)
            (define-key m (kbd "C-|") 'split-window-horizontally)
            m)
  :global t)

(easy-window-split 1)
