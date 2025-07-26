(setq jit-lock-contextually t)

(put 'minibuffer-prompt 'face-defface-spec
     (cons '(((type tty))
             :foreground "green")
           (get 'minibuffer-prompt 'face-defface-spec)))

(keymap-global-set "C-x W" (lambda () (interactive) (load-theme 'whiteboard)))
(keymap-global-set "C-x D" (lambda () (interactive) (load-theme 'deeper-blue)))
