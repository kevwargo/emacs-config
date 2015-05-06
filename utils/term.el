(defun bash-cd (dir)
  (term-send-string (get-buffer-process (current-buffer))
                    (concat [3 10] "cd " dir [10])))

(defun multi-term-other-window (dir)
  (let* ((wd default-directory)
         (window (windmove-do-window-select dir))
         (buf-proc (get-buffer-process (window-buffer window))))
    (if (and (eq major-mode 'term-mode)
             (string-match-p "^.*\\(ba\\|c\\|z\\|/\\)sh$"
                             (file-truename (car (process-command buf-proc)))))
        (unless (string= wd default-directory)
          (bash-cd wd))
      (let ((oldbuf (current-buffer))
            (oldwd default-directory))
        (cd wd)
        (multi-term)
        (with-current-buffer oldbuf
          (cd oldwd))))))

(defun multi-term-up ()
  (interactive)
  (multi-term-other-window 'up))

(defun multi-term-down ()
  (interactive)
  (multi-term-other-window 'down))

(defun multi-term-left ()
  (interactive)
  (multi-term-other-window 'left))

(defun multi-term-right ()
  (interactive)
  (multi-term-other-window 'right))
