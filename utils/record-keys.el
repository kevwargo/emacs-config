(require 'cl-seq)

(defvar key-seq-map (make-hash-table :test 'equal))

(defun log-key-seq ()
  (let* ((seq (this-single-command-raw-keys))
         count)
    (unless (or (eq this-command 'self-insert-command)
                (cl-find-if 'consp seq))
      (setq count (1+ (gethash seq key-seq-map 0)))
      (puthash seq count key-seq-map))))

(defun load-key-seq-map ()
  (condition-case err
      (let ((items (with-temp-buffer
                     (insert-file-contents-literally
                      (locate-user-emacs-file ".key-seq-map"))
                     (goto-char (point-min))
                     (read (current-buffer)))))
        (dolist (item items)
          (puthash (car item) (cdr item) key-seq-map)))
    (file-missing nil)
    (error (message "Error during loading key-seq-map: %S" err))))

(defun dump-key-seq-map ()
  (condition-case err
      (let (items)
        (maphash (lambda (k v) (push (cons k v) items))
                 key-seq-map)
        (with-temp-buffer
          (insert "(")
          (dolist (item (sort items
                              (lambda (i1 i2)
                                (> (cdr i1) (cdr i2)))))
            (print item (current-buffer))
            (delete-char -1)
            (insert (format " ; %S" (key-description (car item)))))
          (insert "\n)\n")
          (write-region nil nil (locate-user-emacs-file ".key-seq-map"))))
    (error (message "Error during dumping key-seq-map: %S" err))))

(add-hook 'pre-command-hook 'log-key-seq)
(add-hook 'after-init-hook 'load-key-seq-map)
(add-hook 'kill-emacs-hook 'dump-key-seq-map)
