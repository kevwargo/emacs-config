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

(add-to-list 'magic-mode-alist '(rjsx-check-buffer . rjsx-mode))
