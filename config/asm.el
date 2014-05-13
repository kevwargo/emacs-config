(defun asm-period ()
  "Insert a period; if there's no non-blank characters before it, delete the indentation beforre."
  (interactive)
  (save-excursion
    (skip-syntax-backward " ")
    (when (bolp)
      (delete-horizontal-space)))
  (call-interactively 'self-insert-command))


(defun asm-mode-hook-function ()
  (local-set-key (kbd ".") 'asm-period)
  ;; Don't indent lines that start with '.'
  (defun asm-calculate-indentation ()
    (or
     ;; Flush labels to the left margin.
     (and (looking-at "\\(\\sw\\|\\s_\\)+:") 0)
     ;; Same thing for `;;;' comments.
     (and (looking-at "\\s<\\s<\\s<") 0)
     ;; Simple `;' comments go to the comment-column.
     (and (looking-at "\\s<\\(\\S<\\|\\'\\)") comment-column)
     ;; The rest goes at the first tab stop.
     (and (= (or (char-after) 0) ?.) 0)
     (or (car tab-stop-list) tab-width))))

(add-hook 'asm-mode-hook 'asm-mode-hook-function)


