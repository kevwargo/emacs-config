(defvar minor-modes-order
  '((flymake-mode . 10)))

(defun rearrange-minor-modes-positions (&rest _)
  (setq minor-mode-alist
        (sort minor-mode-alist
              (lambda (this other)
                (let ((this-order (or (cdr (assq (car this) minor-modes-order)) -1))
                      (other-order (or (cdr (assq (car other) minor-modes-order)) -1)))
                  (> this-order other-order))))))

(add-hook 'after-load-functions 'rearrange-minor-modes-positions)
