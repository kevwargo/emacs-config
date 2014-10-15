(setq jit-lock-contextually t)

(put 'minibuffer-prompt 'face-defface-spec
     (cons '(((type tty))
             :foreground "green")
           (get 'minibuffer-prompt 'face-defface-spec)))
