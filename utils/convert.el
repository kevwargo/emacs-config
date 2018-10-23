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

(defun json-to-yaml ()
  (interactive)
  (let ((oldbuf (current-buffer)))
    (with-temp-buffer
      (let ((tempbuf (current-buffer)))
        (with-current-buffer oldbuf
          (when (= (call-process-region (point-min) (point-max)
                                        "j2y"
                                        nil
                                        (list tempbuf nil)
                                        t
                                        "/dev/stdin")
                   0)
            (delete-region (point-min) (point-max))
            (insert (with-current-buffer tempbuf
                      (buffer-string)))
            (yaml-mode)))))))
