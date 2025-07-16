(require 'dash)
(require 'ibuffer)

(defun ibuffer-dwim ()
  (interactive)
  (ibuffer nil nil (if (--any (with-current-buffer it
                                (and (buffer-file-name) (buffer-modified-p)))
                              (buffer-list))
                       '((visiting-file . nil) (modified . nil)))))

(defun ibuffer-delete-marked-exit ()
  (interactive)
  (ibuffer-do-kill-on-deletion-marks)
  (kill-current-buffer))

(keymap-set ibuffer-mode-map "q" 'kill-current-buffer)
(keymap-set ibuffer-mode-map "X" 'ibuffer-delete-marked-exit)

(keymap-global-set "C-x C-b" 'ibuffer-dwim)
