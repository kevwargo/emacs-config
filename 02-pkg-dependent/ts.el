(require 'typescript-mode)

(defvar k-color-chooser-key "C-c C-k")

(defun k-color-chooser (&optional save)
  (interactive (list (equal (this-command-keys)
                            (key-parse k-color-chooser-key))))
  (when-let* ((s (syntax-ppss))
              (str-start (if (nth 3 s) (nth 8 s)))
              (str-val (thing-at-point 'string t))
              (color-start (1+ str-start))
              (color-end (if (> (length str-val) 2)
                             (+ color-start (- (length str-val) 2))))
              (color (buffer-substring-no-properties color-start color-end))
              (new-color (with-temp-buffer
                           (call-process "kcolorchooser" nil (list t nil) nil
                                         (format "--color=%s" color) "--print")
                           (buffer-substring-no-properties (point-min)
                                                           (point-max)))))
    (save-match-data
      (when (string-match "#[a-fA-F0-9]+" new-color)
        (delete-region color-start color-end)
        (goto-char color-start)
        (insert (match-string 0 new-color))
        (if save (save-buffer))))))

(keymap-set typescript-mode-map "C-{" 'embrace-selected-lines)
(keymap-set typescript-mode-map k-color-chooser-key 'k-color-chooser)
