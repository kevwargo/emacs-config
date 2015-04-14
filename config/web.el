(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))



(defun web-mode-keymap-modify ()
  (local-unset-key (kbd "M-;"))
  (local-set-key (kbd "C-d") 'web-mode-comment-or-uncomment)
  (local-set-key (kbd "C-S-d") 'web-mode-comment-or-uncomment))

(add-hook 'web-mode-hook 'web-mode-keymap-modify)
