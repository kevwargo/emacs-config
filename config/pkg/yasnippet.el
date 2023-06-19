(require 'yasnippet)

(yas-global-mode 1)

(define-key yas-minor-mode-map (kbd "TAB") nil)
(define-key yas-minor-mode-map (kbd "<tab>") nil)
(define-key yas-minor-mode-map (kbd "C-c TAB") yas-maybe-expand)
(define-key yas-minor-mode-map (kbd "C-c & l") 'yas-reload-all)

(add-hook 'find-file-hook 'yas-reload-all)
