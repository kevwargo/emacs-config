(defun konsole-cd (dirname)
  (interactive
   (list (ido-read-directory-name "Konsole-cd: ")))
  (let* ((konsctl (executable-find "konsctl"))
         (cmd (if konsctl
                  (list konsctl "cd" dirname)
                (list "konsole-cd" dirname)))
         (buf (generate-new-buffer "*konsole-cd*")))
    (make-process
     :name "konsole-cd"
     :command cmd
     :buffer buf
     :stderr buf
     :sentinel (lambda (p event)
                 (ignore event)
                 (when-let* ((state (process-status p))
                             (code (process-exit-status p))
                             (pbuf (process-buffer p))
                             ((buffer-live-p pbuf))
                             ((memq state '(exit signal))))
                   (if (zerop code)
                       (kill-buffer pbuf)
                     (with-current-buffer pbuf
                       (goto-char (point-min))
                       (insert (format "Konsole-cd process %d%S %s %d:\n"
                                       (process-id p)
                                       (process-command p)
                                       (if (eq state 'signal)
                                           "received signal"
                                         "exited with code")
                                       code)))
                     (pop-to-buffer pbuf)))))))

(keymap-global-set "C-x c" 'konsole-cd)
