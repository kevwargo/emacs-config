(defun delete-word (arg)
  (interactive "p")
  (delete-region (point) (progn (forward-word arg) (point))))

(defun backward-delete-word (arg)
  (interactive "p")
  (delete-word (- arg)))

(defun newline-from-middle-of-line ()
  (interactive)
  (end-of-line)
  (funcall (key-binding (kbd "RET"))))

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

(defun selected-lines ()
  (if (region-active-p)
      (save-excursion
        (let ((start (region-beginning))
              (end (region-end)))
          (goto-char start)
          (setq start (line-beginning-position))
          (goto-char end)
          (setq end (line-end-respecting-newline))
          (list start end)))
    (list (line-beginning-position) (line-end-respecting-newline))))

(defun move-lines (n &optional keep-text)
  "Partly from Ji Han's answer: http://stackoverflow.com/questions/2423834/move-line-region-up-and-down-in-emacs/19378355"
  (destructuring-bind (beg end) (selected-lines)
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
            (forward-line (* (signum n) (1- (abs n)))))
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

(defun list-buffers-dwim (&optional arg)
  (interactive "P")
  (if (eq major-mode 'Buffer-menu-mode)
      (list-buffers arg)
    (switch-to-buffer
     (list-buffers-noselect nil ; it's ignored anyway in `list-buffers--refresh'
                            (remove-if
                             (lambda (b)
                               (or (string-match-p "^ " (buffer-name b))
                                   (not (buffer-modified-p b))
                                   (not (buffer-file-name b))))
                             (buffer-list))))))

(defun embrace-selected-lines ()
  (interactive)
  (let ((oldpoint (point))
        (oldmark (mark))
        (mark-was-set-p (use-region-p))
        (oldsize (buffer-size))
        (indent-function (key-binding [9])) ; function on tab key
        increment)
    (destructuring-bind (open close) (selected-lines)
      (when (= (char-before close) 10)
        (setq close (1- close)))
      (deactivate-mark)
      (goto-char open)
      (newline-and-indent)
      (previous-line)
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

(defun escape-non-ascii (arg)
  (interactive "p")
  (let ((begin (if (region-active-p)
                   (region-beginning)
                 (point-min)))
        (end (if (region-active-p)
                 (region-end)
               (point-max)))
        char)
    (save-excursion
      (goto-char begin)
      (while (< (point) end)
        (setq char (char-after))
        (if (< char 128)
            (forward-char)
          (when (> char 65535)
            (setq char (logand char 255)))
          (insert (format (if arg "&#x%X" "\\u%04X") char))
          (delete-char 1)
          (setq end (+ end 5)))))))

(defun command-all-buffers-same-major-mode (prefixarg &optional command-name)
  (interactive (list current-prefix-arg (read-extended-command)))
  (let ((original-major-mode major-mode))
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when (eq major-mode original-major-mode)
          (execute-extended-command prefixarg command-name))))))

(defun buffer-working-directory ()
  (if (buffer-file-name)
      (file-name-directory (buffer-file-name))
    default-directory))

(defun pcd ()
  "Print directory name containing current file (if no file, acts like `pwd')"
  (interactive)
  (message (buffer-working-directory)))

(defun normalize-buffer-working-directory ()
  (interactive)
  (let ((cwd (buffer-working-directory)))
    (message "Setting working directory to %S" cwd)
    (cd cwd)))

(defun insert-random-words (arg)
  (interactive "P")
  (let ((words))
    (dotimes (i (cond
                 ((numberp arg) arg)
                 ((consp arg) (car arg))
                 (t (+ 4 (random 50)))))
      (let ((word ""))
        (dotimes (j (1+ (random 20)))
          (setq word (concat word (list
                                   (let ((n (random 64)))
                                     (cond
                                      ((< n 10) (+ n ?0))
                                      ((< n 36) (+ (- n 10) ?A))
                                      ((< n 62) (+ (- n 36) ?a))
                                      ((= n 62) ?_)
                                      (t ?-)))))))
        (push word words)))
    (insert (mapconcat 'identity words " "))))

(defun insert-line-number ()
  (interactive)
  (insert
   (format "%d"
           (save-excursion
             (beginning-of-line)
             (1+ (count-lines 1 (point)))))))

(defun rename-current-file (newname)
  "Rename the file associated with the current buffer to NEWNAME"
  (interactive "sRename current file to: ")
  (let ((oldname (buffer-file-name (current-buffer))))
    (if oldname
        (let ((default-directory (file-name-directory oldname)))
          (rename-file oldname newname)
          (find-alternate-file newname)))))

(defun imv-open (filename)
  (interactive
   (list (ido-read-file-name "imv-open: ")))
  (start-process "imv" nil "imv" filename))

(defun open-stumpwmrc ()
  (interactive)
  (find-file (file-truename "~/.stumpwmrc")))

(defun md5-line-or-region ()
  (interactive)
  (message
   (md5 (apply 'buffer-substring-no-properties (selected-lines)))))
