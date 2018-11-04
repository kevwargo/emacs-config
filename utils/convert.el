(defun escape-non-ascii (arg)
  (interactive "P")
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

(defun convert-buffer (cmd args &optional new-major-mode)
  (let ((oldbuf (current-buffer)))
    (with-temp-buffer
      (let ((tempbuf (current-buffer)))
        (with-current-buffer oldbuf
          (if (/= (apply 'call-process-region (point-min) (point-max)
                         cmd nil tempbuf t args)
                  0)
              (message "j2y convert error: %s" (with-current-buffer tempbuf
                                                 (buffer-string)))
            (delete-region (point-min) (point-max))
            (insert (with-current-buffer tempbuf
                      (buffer-string)))
            (if new-major-mode
                (funcall new-major-mode))))))))

(defun json-to-yaml ()
  (interactive)
  (convert-buffer "j2y" '("/dev/stdin") 'yaml-mode))

(defun yaml-to-json ()
  (interactive)
  (convert-buffer "j2y" '("-r" "/dev/stdin") 'json-mode))

(defun xmllint-buffer ()
  (interactive)
  (convert-buffer "xmllint" '("--format" "-")))
