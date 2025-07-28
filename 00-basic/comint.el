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

(defun comint-run-network (name host service)
  (interactive "sName: \nsHost: \nsPort: ")
  (switch-to-buffer
   (make-comint name (cons host service))))

(defun comint-tuxguitar ()
  (interactive)
  (comint-run-network "tuxguitar" "localhost" 39012))

(defun comint-mode-keymap-modify ()
  (keymap-local-set "<up>" 'comint-up-or-prev-input)
  (keymap-local-set "<down>" 'comint-down-or-next-input)
  (keymap-local-unset "C-c C-x"))

(add-hook 'comint-mode-hook 'comint-mode-keymap-modify)
