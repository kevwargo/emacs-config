(require 'flymake)

(define-key flymake-mode-map (kbd "C-x .") 'flymake-goto-next-error)
(define-key flymake-mode-map (kbd "C-x ,") 'flymake-goto-prev-error)
