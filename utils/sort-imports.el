(defvar-local sort-imports:filters nil)
(defvar-local sort-imports:import-line-regex nil)
(defvar-local sort-imports:separate-groups nil)

(defun sort-imports (beg end)
  (interactive (selected-lines))
  (if (not (consp sort-imports:filters))
      (error "No import filters defined")
    (let* ((lines (remove-if 'string-empty-p
                             (split-string (buffer-substring-no-properties beg end) "\n")))
           groups
           sorted
           last-group)
      (setq groups (mapcar (lambda (line)
                             (let ((filters sort-imports:filters)
                                   group)
                               (while filters
                                 (if (not (string-match-p (car (car filters))
                                                          line))
                                     (setq filters (cdr filters))
                                   (setq group (cdr (car filters)))
                                   (setq filters nil)))
                               (cons group line)))
                           lines))
      (setq sorted (sort groups
                         (lambda (a b)
                           (cond
                            ((eq (car a) (car b))
                             (string< (cdr a) (cdr b)))
                            ((not (car a))
                             nil)
                            ((not (car b))
                             t)
                            ((< (car a) (car b))
                             t)))))
      (delete-region beg end)
      (goto-char beg)
      (mapc (lambda (entry)
              (and sort-imports:separate-groups
                   last-group
                   (not (equalp last-group (car entry)))
                   (insert "\n"))
              (insert (cdr entry))
              (insert "\n")
              (setq last-group (car entry)))
            sorted))))
      
