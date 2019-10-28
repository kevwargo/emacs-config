(require 'yasnippet)

(yas-global-mode 1)

(define-key yas-minor-mode-map (kbd "TAB") nil)
(define-key yas-minor-mode-map (kbd "<tab>") nil)
(define-key yas-minor-mode-map (kbd "C-c TAB") yas-maybe-expand)
(define-key yas-minor-mode-map (kbd "C-c & l") 'yas-reload-all)

(defun yas-disable-ac-tab ()
  (define-key ac-completing-map (kbd "<tab>") nil)
  (define-key ac-completing-map (kbd "TAB") nil))

(defun yas-enable-ac-tab ()
  (define-key ac-completing-map (kbd "<tab>") 'ac-expand)
  (define-key ac-completing-map (kbd "TAB") 'ac-expand))


(add-hook 'yas-before-expand-snippet-hook 'yas-disable-ac-tab)
(add-hook 'yas-after-exit-snippet-hook 'yas-enable-ac-tab)
(add-hook 'find-file-hook 'yas-reload-all)
