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

(defun -j2y-convert-buffer (cmd args &optional new-major-mode)
  (interactive (let ((shell-cmd (read-shell-command "Convert command: "))
                     (new-major-mode (read-string "New major mode (leave empty to use the same): ")))
                 (list shell-file-name
                       (list "-c" shell-cmd)
                       (and (not (string= new-major-mode ""))
                            (intern new-major-mode)))))
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
  (-j2y-convert-buffer "j2y" '("/dev/stdin") 'yaml-mode))

(defun yaml-to-json ()
  (interactive)
  (-j2y-convert-buffer "j2y" '("-r" "/dev/stdin") 'json-mode))

(defun xmllint-buffer ()
  (interactive)
  (-j2y-convert-buffer "xmllint" '("--format" "-")))

(defun decode-base64 (beg end &optional replace)
  "Decodes the region using ULR-safe Base64 encoding.
If the REPLACE is non-nil, replace the region with the resulting text.
Otherwise print the text using `message'."
  (interactive "r\nP")
  (let ((text (base64-decode-string
               (buffer-substring-no-properties beg end)
               t)))
    (scratch-log-expr replace)
    (if replace (progn
                  (delete-region beg end)
                  (goto-char beg)
                  (insert text))
      (message text))))
