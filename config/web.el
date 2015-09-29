(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))



(defun web-mode-keymap-modify ()
  (local-unset-key (kbd "M-;"))
  (local-set-key (kbd "C-{") 'c-embrace-selected-lines)
  (local-set-key (kbd "C-d") 'web-mode-comment-or-uncomment)
  (local-set-key (kbd "C-S-d") 'web-mode-comment-or-uncomment))

(defun web-mode-hook-misc ()
  (electric-indent-mode 0)
  (setq electric-pair-pairs (append electric-pair-pairs (list '(?' . ?'))))
  (setq indent-tabs-mode t))

(add-hook 'web-mode-hook 'web-mode-keymap-modify)
(add-hook 'web-mode-hook 'web-mode-hook-misc)
