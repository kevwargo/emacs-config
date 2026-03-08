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
  "Builds a string from multiple parts, each of which can be separately fontified.

Each ARG is either:
- a string, which is used verbatim,
- a list of form (FACE FMT FMT-ARGS...) where FACE is a non-quoted symbol,
- a list of form (FMT FMT-ARGS...) where FMT is a literal string,
- any other expression will be eval'ed and its value concatenated with the rest."
  `(concat
    ,@(mapcar
       (lambda (arg)
         (if (listp arg)
             (cond ((and (symbolp (car arg))
                         (get (car arg) 'face-defface-spec)
                         (cdr arg))
                    `(string-fontify ,(if (cddr arg)
                                          `(format ,@(cdr arg))
                                        (cadr arg))
                                     ',(car arg)))
                   ((stringp (car arg))
                    `(format ,@arg))
                   (t arg))
           arg))
       args)))
