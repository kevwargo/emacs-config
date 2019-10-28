(defun rjsx-check-buffer ()
  (and
   (string-match-p ".*\\.js$" (buffer-file-name))
   (> (let ((total-xml-size 0)
            match-open)
        (while (setq match-open
                     (and (re-search-forward "<\\s-*\\([a-zA-Z0-9_]+\\).*?>" nil t)
                          (match-beginning 0)))
          (let ((tagname (substring-no-properties (match-string 1)))
                match-close)
            (when (setq match-close
                        (and (re-search-forward (concat "</\\s-*" tagname "\\s-*>") nil t)
                             (match-end 0)))
              (setq total-xml-size (+ total-xml-size (- match-close match-open))))))
        total-xml-size)
      0)))

(defun rjsx-tabify ()
  (save-mark-and-excursion
    (goto-char (point-min))
    (while (re-search-forward
            (format "^\\(\\( \\{%d\\}\\|\t\\)+\\)\\( \\{0,%d\\}[^ \t]\\)"
                    tab-width
                    (1- tab-width))
            nil t)
      (let* ((len (current-indentation))
             (replacement (concat
                           (make-string (/ len tab-width) 9)
                           "\\3")))
        (replace-match replacement)))))

(defun setup-rjsx-mode ()
  (local-unset-key (kbd "C-d"))
  (local-unset-key (kbd "C-S-d"))
  (local-set-key (kbd "C-x w") 'rjsx-element-wrap)
  ;; (add-hook 'before-save-hook 'rjsx-tabify nil t)
  )

(defun rjsx-element-wrap (beg end &optional tag-name)
  (interactive "r")
  (unless tag-name
    (setq tag-name (read-from-minibuffer "Tag name? ")))
  (let ((sep (if (memq (get-text-property beg 'rjsx-class)
                       '(< >))
                 "\n"
               "")))
    (insert-at end (concat sep "</" tag-name ">"))
    (insert-at beg (concat "<" tag-name ">" sep))
    (when (string= sep "\n")
      (indent-region beg (+ end (* (+ 3 (length tag-name)) 2)))))
  (deactivate-mark))


(add-to-list 'magic-mode-alist '(rjsx-check-buffer . rjsx-mode))
(add-hook 'rjsx-mode-hook 'setup-rjsx-mode)
