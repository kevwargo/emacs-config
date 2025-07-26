(require 'flymake)

(keymap-set flymake-mode-map "C-x ." 'flymake-goto-next-error)
(keymap-set flymake-mode-map "C-x ," 'flymake-goto-prev-error)
