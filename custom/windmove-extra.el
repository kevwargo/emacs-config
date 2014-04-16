(require 'windmove)

(scratch-log "%S" load-file-name)

(defun find-other-window-dir (dir)
  "Wrapper for `windmove-find-other-window'. When window in direction DIR
can't be selected, it yields an error instead of returning nil."
  (let ((window (windmove-find-other-window dir)))
    (cond
     ((not window)
      (error "No window %s from selected window" dir))
     ((and (window-minibuffer-p window)
           (not (minibuffer-window-active-p window)))
      (error "Minibuffer is inactive"))
     (t window))))

(defun insert-to-buffer-dir (dir beg end &optional cut)
  "Inserts selected text to the buffer in the window in direction DIR
and selects that window. If CUT is non-nil, deletes selected text in current buffer."
  (let ((text (if (not (region-active-p))
                  (error "No active region")
                (buffer-substring-no-properties beg end)))
        (window (find-other-window-dir dir)))
    (when cut
      (barf-if-buffer-read-only)
      (delete-region beg end))
    (select-window window)
    (barf-if-buffer-read-only)
    (insert text)))

(defun copy-to-buffer-left (beg end)
  "Inserts selected text to the buffer in the window to the left of current
and selects that window."
  (interactive "r")
  (insert-to-buffer-dir 'left beg end))

(defun copy-to-buffer-right (beg end)
  "Inserts selected text to the buffer in the window to the right of current
and selects that window."
  (interactive "r")
  (insert-to-buffer-dir 'right beg end))

(defun copy-to-buffer-up (beg end)
  "Inserts selected text to the buffer in the window above current
and selects that window."
  (interactive "r")
  (insert-to-buffer-dir 'up beg end))

(defun copy-to-buffer-down (beg end)
  "Inserts selected text to the buffer in the window below current
and selects that window."
  (interactive "r")
  (insert-to-buffer-dir 'down beg end))

(defun cut-to-buffer-left (beg end)
  "Cuts selected text to the buffer in the window to the left of current
and selects that window."
  (interactive "r")
  (insert-to-buffer-dir 'left beg end t))

(defun cut-to-buffer-right (beg end)
  "Cuts selected text to the buffer in the window to the right of current
and selects that window."
  (interactive "r")
  (insert-to-buffer-dir 'right beg end t))

(defun cut-to-buffer-up (beg end)
  "Cuts selected text to the buffer in the window above current
and selects that window."
  (interactive "r")
  (insert-to-buffer-dir 'up beg end t))

(defun cut-to-buffer-down (beg end)
  "Cuts selected text to the buffer in the window below current
and selects that window."
  (interactive "r")
  (insert-to-buffer-dir 'down beg end t))

(defun swap-buffers-dir (dir)
  (let ((this-buffer (current-buffer)))
    (switch-to-buffer (window-buffer (find-other-window-dir dir)))
    (windmove-do-window-select dir)
    (switch-to-buffer this-buffer)))

(defun swap-buffers-left ()
  (interactive)
  (swap-buffers-dir 'left))

(defun swap-buffers-right ()
  (interactive)
  (swap-buffers-dir 'right))

(defun swap-buffers-up ()
  (interactive)
  (swap-buffers-dir 'up))

(defun swap-buffers-down ()
  (interactive)
  (swap-buffers-dir 'down))

(defun move-buffer-dir (dir)
  (let ((buf (current-buffer))
        (window (find-other-window-dir dir)))
    (previous-buffer)
    (windmove-do-window-select dir)
    (switch-to-buffer buf)))

(defun move-buffer-left ()
  (interactive)
  (move-buffer-dir 'left))

(defun move-buffer-right ()
  (interactive)
  (move-buffer-dir 'right))

(defun move-buffer-up ()
  (interactive)
  (move-buffer-dir 'up))

(defun move-buffer-down ()
  (interactive)
  (move-buffer-dir 'down))

(defun find-file-other-window-dir (dir filename &optional wildcards)
  (let* ((cwd default-directory)
         (oldbuf (window-buffer (find-other-window-dir dir)))
         (oldwd (with-current-buffer oldbuf default-directory)))
    (windmove-do-window-select dir)
    (cd cwd)
    (find-file filename wildcards)
    (with-current-buffer oldbuf
      (cd oldwd))))

(defun find-file-other-window-left (filename &optional wildcards)
  (interactive
   (find-file-read-args "Find file: "
                        (confirm-nonexistent-file-or-buffer)))
  (find-file-other-window-dir 'left filename wildcards))

(defun find-file-other-window-right (filename &optional wildcards)
  (interactive
   (find-file-read-args "Find file: "
                        (confirm-nonexistent-file-or-buffer)))
  (find-file-other-window-dir 'right filename wildcards))

(defun find-file-other-window-up (filename &optional wildcards)
  (interactive
   (find-file-read-args "Find file: "
                        (confirm-nonexistent-file-or-buffer)))
  (find-file-other-window-dir 'up filename wildcards))

(defun find-file-other-window-down (filename &optional wildcards)
  (interactive
   (find-file-read-args "Find file: "
                        (confirm-nonexistent-file-or-buffer)))
  (find-file-other-window-dir 'down filename wildcards))

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


(defun ielm-other-window (dir)
  (let ((buf (current-buffer)))
    (windmove-do-window-select dir)
    (ielm)
    (ielm-change-working-buffer buf)))

(defun ielm-up ()
  (interactive)
  (ielm-other-window 'up))

(defun ielm-down ()
  (interactive)
  (ielm-other-window 'down))

(defun ielm-left ()
  (interactive)
  (ielm-other-window 'left))

(defun ielm-right ()
  (interactive)
  (ielm-other-window 'right))
