(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.inc\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.xml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))


(defun web-mode-beginning-of-defun (&optional arg)
  (interactive "^p")
  (let* ((language (plist-get (web-mode-point-context (point)) :language))
         (beginning-of-defun-function-orig beginning-of-defun-function)
         (beginning-of-defun-function
          (cond ((string= language "php") 'php-beginning-of-defun)
                ;; ((string= language "javascript") 'js-beginning-of-defun)
                (t beginning-of-defun-function-orig))))
    ;; (scratch-log "beg defun %S" beginning-of-defun-function)
    (beginning-of-defun arg)))

(defun web-mode-end-of-defun (&optional arg)
  (interactive "^p")
  (let* ((language (plist-get (web-mode-point-context (point)) :language))
         (end-of-defun-function-orig end-of-defun-function)
         (end-of-defun-function
          (cond ((string= language "php") 'php-end-of-defun)
                ;; ((string= language "javascript") 'js-end-of-defun)
                (t end-of-defun-function-orig))))
    ;; (scratch-log "end defun %S" end-of-defun-function)
    (end-of-defun arg)))


(defun web-mode-keymap-modify ()
  (local-unset-key (kbd "M-;"))
  (local-set-key (kbd "C-{") 'embrace-selected-lines)
  (local-set-key (kbd "C-d") 'web-mode-comment-or-uncomment)
  (local-set-key (kbd "C-M-a") 'web-mode-beginning-of-defun)
  (local-set-key (kbd "C-M-e") 'web-mode-end-of-defun)
  (local-set-key (kbd "C-S-d") 'web-mode-comment-or-uncomment))

(defun web-mode-hook-misc ()
  (setq electric-pair-pairs (append electric-pair-pairs (list '(?' . ?'))))
  ;; (if (assoc "lineup-args" web-mode-indentation-params)
  ;;     (setcdr (assoc "lineup-args" web-mode-indentation-params) nil)
  ;;   (add-to-list 'web-mode-indentation-params '("lineup-args" . nil)))
  )

(add-hook 'web-mode-hook 'web-mode-keymap-modify)
(add-hook 'web-mode-hook 'web-mode-hook-misc)
