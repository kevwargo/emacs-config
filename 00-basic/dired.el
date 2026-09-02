(with-eval-after-load 'dired
  (keymap-set dired-mode-map "DEL" #'dired-up-directory))
