(require 'dash)

(defun ibuffer-dwim ()
  (interactive)
  (ibuffer nil nil (if (--any (with-current-buffer it
                                (and (buffer-file-name) (buffer-modified-p)))
                              (buffer-list))
                       '((visiting-file . nil) (modified . nil)))))

(keymap-set ibuffer-mode-map "q" 'kill-current-buffer)

(keymap-global-set "C-x C-b" 'ibuffer-dwim)
