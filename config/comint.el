(defun comint-previous-input-straight (arg)
  "Move backwards in input history (without cycling)"
  (interactive "*p")
  (cond
   ((and comint-input-ring-index
         (< arg 0)
         (eq comint-input-ring-index 0)
         comint-stored-incomplete-input)
    (comint-restore-input))
   ((or (and (< arg 0)
             (null comint-input-ring-index))
        (and (> arg 0)
             (eq comint-input-ring-index
                 (1- (ring-length comint-input-ring)))))
    nil)
   (t
    (comint-previous-matching-input "." arg))))

(defun comint-next-input-straight (arg)
  "Move forward in input history (without cycling)"
  (interactive "*p")
  (comint-previous-input-straight (- arg)))


(defun comint-up-or-prev-input (&optional arg)
  (interactive "*^p")
  (if (and (eobp)
           (comint-after-pmark-p))
      (comint-previous-input-straight arg)
    (previous-line arg)))

(defun comint-down-or-next-input (&optional arg)
  (interactive "*^p")
  (if (and (eobp)
           (comint-after-pmark-p))
      (comint-next-input-straight arg)
    (next-line arg)))

(defun comint-mode-keymap-modify ()
  ;; (local-set-key [up] 'comint-previous-input)
  ;; (local-set-key [down] 'comint-next-input)
  (local-set-key [up] 'comint-up-or-prev-input)
  (local-set-key [down] 'comint-down-or-next-input)
  (local-set-key (kbd "C-<up>") 'previous-line)
  (local-set-key (kbd "C-<down>") 'next-line)
  (dolist (key '("l" "a" "s" "w" "d" "x"))
    (eval `(local-unset-key (kbd ,(concat "C-c C-" key))))))

(add-hook 'comint-mode-hook 'comint-mode-keymap-modify)
