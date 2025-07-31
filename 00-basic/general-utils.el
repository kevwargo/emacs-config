(require 'cl-lib)
(require 'cl-extra)

(defun delete-word (arg)
  (interactive "p")
  (delete-region (point) (progn (forward-word arg) (point))))

(defun backward-delete-word (arg)
  (interactive "p")
  (delete-word (- arg)))

(defun newline-from-middle-of-line ()
  (interactive)
  (end-of-line)
  (funcall (key-binding (key-parse "RET"))))

(defun delete-line (arg)
  (interactive "p")
  (if (and (> arg 0) (eobp) (save-excursion (forward-visible-line 0) (eobp)))
      (signal 'end-of-buffer nil))
  (if (and (< arg 0) (bobp) (save-excursion (end-of-visible-line) (bobp)))
      (signal 'beginning-of-buffer nil))
  (cond ((zerop arg))
        ((< arg 0)
         (end-of-visible-line)
         (delete-region (point)
                        (progn (forward-visible-line (1+ arg))
                               (unless (bobp) (backward-char))
                               (point))))
        (t
         (forward-visible-line 0)
         (delete-region (point)
                        (progn (forward-visible-line arg)
                               (point))))))

(defun smart-set-mark-command (arg)
  (interactive "P")
  (if (not (region-active-p))
      (set-mark-command arg)
    (deactivate-mark)
    (message "Mark unset")))

(defun smart-beginning-of-line ()
  "Move point to first non-whitespace character or beginning-of-line.

Move point to the first non-whitespace character on this line.
If point was already at that position, move point to beginning of line."
  (interactive "^") ; Use (interactive "^") in Emacs 23 to make shift-select work
  (let ((oldpos (point)))
    (back-to-indentation)
    (and (= oldpos (point))
         (beginning-of-line))))

(defun insert-tab-command (&optional arg)
  (interactive "P")
  (insert-tab arg))

(defun line-end-respecting-newline ()
  ;; don't use last line if just '\n'-char preceding it is selected
  (+ (line-end-position)
     (save-excursion
       (goto-char (line-end-position))
       (if (eobp) 0 1))))

(defun selected-lines (&optional as-cons)
  (let ((collect-fn (if as-cons 'cons 'list)))
    (if (use-region-p)
        (save-excursion
          (let ((start (region-beginning))
                (end (region-end)))
            (goto-char start)
            (setq start (line-beginning-position))
            (goto-char end)
            (setq end (line-end-respecting-newline))
            (funcall collect-fn start end)))
      (funcall collect-fn (line-beginning-position) (line-end-respecting-newline)))))

(defun move-lines (n &optional keep-text)
  "Partly from Ji Han's answer: http://stackoverflow.com/questions/2423834/move-line-region-up-and-down-in-emacs/19378355"
  (cl-destructuring-bind (beg end) (selected-lines)
    (when (and (= end (point-max))
               (/= (char-before end) 10))
      (save-excursion
        (goto-char end)
        (insert-char 10)
        (setq end (1+ end))))
    (let ((keep-selection (region-active-p))
          (offset (if (and (mark t)
                           (>= (mark t) beg)
                           (< (mark t) end))
                      (- (point) (mark t))))
          (rewind (- end (point)))
          text)
      (goto-char (if (< n 0) beg end))
      (if keep-text
          (progn
            (setq text (buffer-substring beg end))
            (forward-line (* (cl-signum n) (1- (abs n)))))
        (setq text (delete-and-extract-region beg end))
        (forward-line n))
      (insert text)
      (backward-char rewind)
      (if offset (push-mark (- (point) offset)))
      (if keep-selection
          (setq mark-active t
                deactivate-mark nil)))))

(defun move-lines-up (n)
  "move the line(s) spanned by the active region up by N lines."
  (interactive "*p")
  (move-lines (- (or n 1))))

(defun move-lines-down (n)
  "move the line(s) spanned by the active region down by N lines."
  (interactive "*p")
  (move-lines (or n 1)))

(defun copy-lines-up (n)
  (interactive "*p")
  (move-lines (- (or n 1)) t))

(defun copy-lines-down (n)
  (interactive "*p")
  (move-lines (or n 1) t))

(defun comment-lines (arg)
  (interactive "*p")
  (apply 'comment-region (selected-lines)))

(defun uncomment-lines (arg)
  (interactive "*p")
  (apply 'uncomment-region (selected-lines)))

(defun embrace-selected-lines ()
  (interactive)
  (let ((oldpoint (point))
        (oldmark (mark))
        (mark-was-set-p (use-region-p))
        (oldsize (buffer-size))
        (indent-function (key-binding [9])) ; function on tab key
        increment)
    (cl-destructuring-bind (open close) (selected-lines)
      (when (= (char-before close) 10)
        (setq close (1- close)))
      (deactivate-mark)
      (goto-char open)
      (newline-and-indent)
      (forward-line -1)
      (insert "{")
      (funcall indent-function)
      (setq increment (- (buffer-size) oldsize))
      (goto-char (setq close (+ increment close)))
      (newline-and-indent)
      (insert "}")
      (funcall indent-function)
      (save-excursion
        (goto-char (+ increment open))
        (set-mark close)
        (funcall indent-function)))
    (goto-char (+ oldpoint increment))
    (when mark-was-set-p
      (set-mark (+ oldmark increment)))))

(defun command-all-buffers-same-major-mode (command-name)
  (interactive (list (read-extended-command)))
  (let ((original-major-mode major-mode))
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when (eq major-mode original-major-mode)
          (command-execute command-name))))))

(defun buffer-working-directory (&optional buffer)
  (with-current-buffer (or buffer (current-buffer))
    (if (buffer-file-name)
        (file-name-directory (buffer-file-name))
      default-directory)))

(defun pcd ()
  "Print directory name containing current file (if no file, acts like `pwd')"
  (interactive)
  (message (buffer-working-directory)))

(defun normalize-buffer-working-directory ()
  (interactive)
  (let ((cwd (buffer-working-directory)))
    (message "Setting working directory to %S" cwd)
    (cd cwd)))

(defun mv (old-name new-name)
  (interactive (let ((old-name (or (buffer-file-name)
                                   (user-error "Buffer %S is not visiting a file" (current-buffer))))
                     (ido-file-completion-map (let ((m (make-sparse-keymap)))
                                                (set-keymap-parent m ido-file-completion-map)
                                                (keymap-set m "C-c C-c" 'ido-select-text)
                                                m)))
                 (list old-name (ido-read-file-name "Move/rename to: "))))
  (if (file-directory-p new-name)
      (setq new-name (expand-file-name (file-name-nondirectory old-name)
                                       new-name)))
  (rename-file old-name new-name
               1 ;; from doc-string: An integer third arg means request confirmation if NEWNAME already exists.
               )
  (message "Moved/renamed %S -> %S" old-name new-name)
  (find-alternate-file new-name))

(defun imv-open (filename)
  (interactive
   (list (ido-read-file-name "imv-open: ")))
  (start-process "imv-x11" nil "imv" filename))

(defun md5-line-or-region ()
  (interactive)
  (message
   (md5 (apply 'buffer-substring-no-properties
               (if (region-active-p)
                   (list
                    (region-beginning)
                    (region-end))
                 (list
                  (line-beginning-position)
                  (line-end-respecting-newline)))))))

(defun make-current-file-writable ()
  (interactive)
  (let ((file (buffer-file-name)))
    (when file
      (if (not (= (call-process "sudo" nil nil nil "chmod" "a+rw" file) 0))
          (message "Cannot chmod file %S" file)))))

(defun find-file-from-buffer ()
  (interactive)
  (let* ((buf (ido-read-buffer "Buffer: " nil t))
         (default-directory (buffer-working-directory buf)))
    (ido-find-file)))

(defun mark-current-sexp ()
  (interactive)
  (cl-destructuring-bind (beg . end)
      (bounds-of-thing-at-point 'sexp)
    (push-mark beg nil t)
    (goto-char end)))

(defun mark-current-word ()
  (interactive)
  (cl-destructuring-bind (beg . end)
      (bounds-of-thing-at-point 'word)
    (goto-char beg)
    (push-mark beg nil t)
    (goto-char end)))

(defun mark-current-line (arg)
  "Mark current line.

If prefix argument ARG is not nil, mark from the beginning of line
instead of from the first non-whitespace character"
  (interactive "P")
  (beginning-of-line)
  (unless arg
    (back-to-indentation))
  (push-mark (point) nil t)
  (goto-char (line-end-position)))

(defun find-file-in-kec ()
  (interactive)
  (let ((default-directory kec-config-dir))
    (ido-find-file)))

(defun file-name-to-clipboard ()
  (interactive)
  (when-let ((filename (buffer-file-name)))
    (with-temp-buffer
      (insert filename)
      (kill-ring-save (point-min) (point-max)))))

(defun sudo-save-buffer ()
  (interactive)
  (when-let ((filename (shell-quote-argument (buffer-file-name)))
             (tmpfile (make-temp-file "emacs-sudofile")))
    (unwind-protect
        (progn
          (write-region (point-min) (point-max) tmpfile)
          (with-temp-buffer
            (let* ((cmd (format "sudo -S cp %s %s && sudo -S chown %d:%d %s"
                                tmpfile
                                filename
                                (user-uid)
                                (group-gid)
                                filename))
                   (pwd (read-passwd (concat cmd ": "))))
              (insert pwd)
              (shell-command-on-region (point-min) (point-max) cmd
                                       (current-buffer) t nil t)))
          (set-buffer-modified-p nil))
      (delete-file tmpfile))))

(defun to-camel-case (string)
  (let ((parts (split-string string "_")))
    (apply #'concat
           (append (list (car parts))
                   (mapcar #'capitalize (cdr parts))))))

(defun to-camel-case-at-point ()
  (interactive)
  (let ((bounds (bounds-of-thing-at-point 'sexp))
        sexp)
    (when bounds
      (cl-destructuring-bind (start . end)
          bounds
        (setq sexp (buffer-substring-no-properties start end))
        (goto-char start)
        (delete-region start end)
        (insert (to-camel-case sexp))))))
