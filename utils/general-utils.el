(defun delete-word (arg)
  (interactive "p")
  (delete-region (point) (progn (forward-word arg) (point))))

(defun backward-delete-word (arg)
  (interactive "p")
  (delete-word (- arg)))

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
  "Partly by Ji Han from StackOverflow.com."
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


