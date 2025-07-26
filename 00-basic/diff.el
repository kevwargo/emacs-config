(defun smerge-ignore-whitespace (&optional reverse)
  (interactive "P")
  (let ((old-file (make-temp-file "emacs-diff-w-old-"))
        (new-file (make-temp-file "emacs-diff-w-new-"))
        (buf (get-buffer-create "*diff-no-whitespace*")))
    (ignore-errors
      (smerge-match-conflict)
      (if reverse
          (progn
            (write-region (match-beginning 3) (match-end 3) old-file)
            (write-region (match-beginning 1) (match-end 1) new-file))
        (write-region (match-beginning 1) (match-end 1) old-file)
        (write-region (match-beginning 3) (match-end 3) new-file))
      (with-current-buffer buf
        (erase-buffer)
        (call-process "diff" nil t t "-w" "-u" old-file new-file)
        (goto-char (point-min))
        (diff-mode)
        (ignore-errors
          (save-excursion
            (while t
              (diff-hunk-next)
              (diff-refine-hunk)))))
      (switch-to-buffer-other-window buf)
    (if (file-exists-p old-file)
        (delete-file old-file))
    (if (file-exists-p new-file)
        (delete-file new-file)))))

(defun smerge-custom-keys ()
  (keymap-local-set "C-c w" 'smerge-ignore-whitespace))

(add-hook 'smerge-mode-hook 'smerge-custom-keys)
