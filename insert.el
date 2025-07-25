(defun insert-current-date ()
  (interactive)
  (insert (format-time-string "%Y-%m-%d_%H%M%S")))

(defun insert-random-words (arg)
  (interactive "P")
  (let ((words))
    (dotimes (i (cond
                 ((numberp arg) arg)
                 ((consp arg) (car arg))
                 (t (+ 4 (random 50)))))
      (let ((word ""))
        (dotimes (j (1+ (random 20)))
          (setq word (concat word (list
                                   (let ((n (random 64)))
                                     (cond
                                      ((< n 10) (+ n ?0))
                                      ((< n 36) (+ (- n 10) ?A))
                                      ((< n 62) (+ (- n 36) ?a))
                                      ((= n 62) ?_)
                                      (t ?-)))))))
        (push word words)))
    (insert (mapconcat 'identity words " "))))

(defun insert-line-number ()
  (interactive)
  (insert
   (format "%d"
           (save-excursion
             (beginning-of-line)
             (1+ (count-lines 1 (point)))))))

(defun insert-at (pos text)
  (save-excursion
    (goto-char pos)
    (insert text)))
