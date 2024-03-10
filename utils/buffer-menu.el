(require 'cl-seq)

(defun list-buffers-dwim (&optional arg)
  (interactive)
  (switch-to-buffer
   (list-buffers-noselect nil ; it's ignored anyway in `list-buffers--refresh'
                          (cl-remove-if
                           (lambda (b)
                             (or (string-match-p "^ " (buffer-name b))
                                 (not (buffer-modified-p b))
                                 (not (buffer-file-name b))))
                           (buffer-list)))))

(defun buffer-menu-execute-kill ()
  (interactive)
  (Buffer-menu-execute)
  (kill-buffer))

(defun buffer-menu-filter-major-mode (mode-names)
  (interactive
   (list
    (ido-completing-read "Filter by major mode: "
                         (mapcar (lambda (b)
                                   (with-current-buffer b
                                     (symbol-name major-mode)))
                                 (buffer-list)))))
  (let* ((mode-names (if (listp mode-names)
                         mode-names
                       (list mode-names)))
         (buffers (cl-remove-if-not (lambda (b)
                                  (cl-find (with-current-buffer b
                                          (symbol-name major-mode))
                                        mode-names
                                        :test 'string=))
                                 (buffer-list))))
    (if buffers
        (list-buffers-noselect nil buffers)
      (message "No buffers with mode `%S'" mode-names))))

(defun buffer-menu-show-orhpaned ()
  (interactive)
  (let ((buffers (cl-remove-if-not (lambda (b)
                                  (and (buffer-file-name b)
                                       (not (file-exists-p (buffer-file-name b)))))
                                (buffer-list))))
    (if buffers
        (list-buffers-noselect nil buffers)
      (message "No buffers for deleted files"))))

(defun buffer-menu-find-alternate-file ()
  (interactive)
  (dolist (buf (Buffer-menu-marked-buffers))
    (with-current-buffer buf
      (find-alternate-file (buffer-file-name)))))


(defun buffer-menu-sort-by-filename ()
  (interactive)
  (tabulated-list-sort 6))

(defun buffer-menu-keys ()
  (local-set-key (kbd "X") 'buffer-menu-execute-kill)
  (local-set-key (kbd "m") 'buffer-menu-filter-major-mode)
  (local-set-key (kbd "O") 'buffer-menu-show-orhpaned)
  (local-set-key (kbd ">") 'Buffer-menu-mark)
  (local-set-key (kbd "V") 'buffer-menu-find-alternate-file)
  (local-set-key (kbd "f") 'buffer-menu-sort-by-filename)
  (local-set-key (kbd "C-x C-b") 'list-buffers))


(add-hook 'Buffer-menu-mode-hook 'buffer-menu-keys)
