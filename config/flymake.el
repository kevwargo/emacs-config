(require 'flymake)

(define-key flymake-mode-map (kbd "C-c .") 'flymake-goto-next-error)
(define-key flymake-mode-map (kbd "C-c ,") 'flymake-goto-prev-error)
