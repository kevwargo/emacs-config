(require 'smerge-mode)

(define-minor-mode smerge-easy-keys-mode
  "Easy keybindings in smerge mode"
  :group 'smerge :lighter " SMEK"
  :keymap `(keymap
            (?i . smerge-easy-keys-mode)
            ,@(cdr smerge-basic-map)))

(defun custom-smerge-mode-keys ()
  (define-key smerge-mode-map (kbd "C-c ^ k") 'smerge-easy-keys-mode))

(add-hook 'smerge-mode-hook 'custom-smerge-mode-keys)
