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

(defun decode-base64 (beg end &optional replace)
  "Decodes the region using ULR-safe Base64 encoding.
If the REPLACE is non-nil, replace the region with the resulting text.
Otherwise print the text using `message'."
  (interactive "r\nP")
  (let ((text (base64-decode-string (buffer-substring-no-properties beg end) t)))
    (if replace (progn
                  (delete-region beg end)
                  (goto-char beg)
                  (insert text))
      (message text))))

(defun string-fontify (str face)
  (if face
      (propertize str 'face face 'font-lock-face face)
    str))

(defmacro format-fontify (&rest args)
  "Builds a string from multiple parts, each of which can be separately fontified."
  (let (concat-args arg face)
    (while args
      (setq arg (pop args))
      (if (and (listp arg) (stringp (car arg)))
          (setq arg `(format ,@arg)))
      (if args
          (setq arg `(string-fontify ,arg ,(pop args))))
      (push arg concat-args))
    `(concat ,@(nreverse concat-args))))
