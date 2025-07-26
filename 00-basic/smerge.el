(require 'smerge-mode)

(define-minor-mode smerge-easy-keys-mode
  "Easy keybindings in smerge mode"
  :group 'smerge
  :lighter " SMEK"
  :keymap `(keymap
            (?i . smerge-easy-keys-mode)
            ,@(cdr smerge-basic-map)))

(keymap-set smerge-mode-map "C-c ^ k" 'smerge-easy-keys-mode)
