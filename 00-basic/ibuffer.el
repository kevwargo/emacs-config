(require 'cl-seq)
(require 'ibuffer)

(defun ibuffer-dwim ()
  (interactive)
  (let ((ibuffer-hook
         `(,(lambda ()
              (setq ibuffer-filtering-qualifiers
                    (if (cl-find-if (lambda (b)
                                      (with-current-buffer b
                                        (and (buffer-file-name) (buffer-modified-p))))
                                    (buffer-list))
                        '((visiting-file . nil) (modified . nil))))
              (ibuffer-update nil t)))))
    (ibuffer)))

(defun ibuffer-delete-marked-exit ()
  (interactive)
  (ibuffer-do-kill-on-deletion-marks)
  (kill-current-buffer))

(keymap-set ibuffer-mode-map "q" 'kill-current-buffer)
(keymap-set ibuffer-mode-map "X" 'ibuffer-delete-marked-exit)

(keymap-global-set "C-x C-b" 'ibuffer-dwim)
