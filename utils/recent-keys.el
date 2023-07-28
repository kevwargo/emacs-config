(defun insert-key-description (key count)
  (insert (key-description (list key))
          (if (> count 1)
              (format "*%d" count)
            "")
          10))

(defun show-recent-keys ()
  (interactive)
  (let ((temp-buffer-window-setup-hook
         (list (lambda ()
                 (let* ((keys (recent-keys))
                        (keys-len (length keys))
                        (prev-counter 1)
                        prev-key)
                   (seq-do-indexed (lambda (key idx)
                                     (cond ((= idx (1- keys-len))
                                            (if (equal key prev-key)
                                                (insert-key-description key (1+ prev-counter))
                                              (insert-key-description key 1)))
                                           ((equal key prev-key)
                                            (setq prev-counter (1+ prev-counter)))
                                           (prev-key
                                            (insert-key-description prev-key prev-counter)
                                            (setq prev-counter 1)))
                                     (setq prev-key key))
                                   keys))
                 (local-set-key (kbd "q") 'kill-this-buffer)))))
    (with-temp-buffer-window "*Recent Keys*" nil nil)))
