;;; -*- lexical-binding: t -*-

(require 'uuidgen)

(defun uuid ()
  (interactive)
  (let ((oldpos (point)))
    (insert (uuidgen-4))
    (set-transient-map (let ((m (make-sparse-keymap)))
                         (keymap-set m "u" (lambda ()
                                             (interactive)
                                             (delete-region oldpos (point))
                                             (insert (uuidgen-4))))
                         m)
                       t nil
                       "Use %k to generate new UUID")))
