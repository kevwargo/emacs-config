(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "M-<left>"))
(global-unset-key (kbd "M-<right>"))
(global-unset-key (kbd "C-x h"))
(global-unset-key (kbd "C-x C-z"))


;; Common keybindins
(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "M-c") 'toggle-case-fold-search)
(global-set-key (kbd "C-q") 'delete-frame)
(global-set-key (kbd "C-k") 'delete-line)
(global-set-key (kbd "C-z") 'undo-tree-undo)
(global-set-key (kbd "C-d") 'comment-lines)
(global-set-key (kbd "C-a") 'mark-whole-buffer)
(global-set-key (kbd "<home>") 'smart-beginning-of-line)
(global-set-key (kbd "C-x k") 'kill-this-buffer)
(global-set-key (kbd "C-x x") 'kmacro-call-macro)
(global-set-key (kbd "C-c r") 'replace-string)
(global-set-key (kbd "C-c C-r") 'replace-regexp)
(global-set-key (kbd "C-c C-x k") 'save-buffers-kill-emacs)
(global-set-key (kbd "C-c C-\\") 'goto-last-change)
(global-set-key (kbd "C-c C-f") 'find-function)
(global-set-key (kbd "C-x DEL") 'join-line)
(global-set-key (kbd "C-x C-b") 'list-buffers-dwim)
(global-set-key (kbd "M-X") 'command-all-buffers-same-major-mode)


(global-set-key (kbd "C-c 1") (make-sparse-keymap))
(global-set-key (kbd "C-c 1 <left>") 'copy-to-buffer-left)
(global-set-key (kbd "C-c 1 <right>") 'copy-to-buffer-right)
(global-set-key (kbd "C-c 1 <up>") 'copy-to-buffer-up)
(global-set-key (kbd "C-c 1 <down>") 'copy-to-buffer-down)

(global-set-key (kbd "C-c 2") (make-sparse-keymap))
(global-set-key (kbd "C-c 2 <left>") 'cut-to-buffer-left)
(global-set-key (kbd "C-c 2 <right>") 'cut-to-buffer-right)
(global-set-key (kbd "C-c 2 <up>") 'cut-to-buffer-up)
(global-set-key (kbd "C-c 2 <down>") 'cut-to-buffer-down)

(global-set-key (kbd "C-c 3") (make-sparse-keymap))
(global-set-key (kbd "C-c 3 <left>") 'swap-buffers-left)
(global-set-key (kbd "C-c 3 <right>") 'swap-buffers-right)
(global-set-key (kbd "C-c 3 <up>") 'swap-buffers-up)
(global-set-key (kbd "C-c 3 <down>") 'swap-buffers-down)

(global-set-key (kbd "C-c 4") (make-sparse-keymap))
(global-set-key (kbd "C-c 4 <left>") 'move-buffer-left)
(global-set-key (kbd "C-c 4 <right>") 'move-buffer-right)
(global-set-key (kbd "C-c 4 <up>") 'move-buffer-up)
(global-set-key (kbd "C-c 4 <down>") 'move-buffer-down)

(global-set-key (kbd "C-x f") (make-sparse-keymap))
(global-set-key (kbd "C-x f <left>") 'find-file-other-window-left)
(global-set-key (kbd "C-x f <right>") 'find-file-other-window-right)
(global-set-key (kbd "C-x f <up>") 'find-file-other-window-up)
(global-set-key (kbd "C-x f <down>") 'find-file-other-window-down)
(global-set-key (kbd "C-x f b") 'find-file-from-buffer)
(global-set-key (kbd "C-x f c") 'find-file-in-kec)

(global-set-key (kbd "C-c t") (make-sparse-keymap))
(global-set-key (kbd "C-c t <up>") 'multi-term-up)
(global-set-key (kbd "C-c t <down>") 'multi-term-down)
(global-set-key (kbd "C-c t <left>") 'multi-term-left)
(global-set-key (kbd "C-c t <right>") 'multi-term-right)

(global-set-key (kbd "C-c d") (make-sparse-keymap))
(global-set-key (kbd "C-c d <up>") 'copy-cwd-up)
(global-set-key (kbd "C-c d <down>") 'copy-cwd-down)
(global-set-key (kbd "C-c d <left>") 'copy-cwd-left)
(global-set-key (kbd "C-c d <right>") 'copy-cwd-right)
(global-set-key (kbd "C-c d x") 'normalize-buffer-working-directory)

(global-set-key (kbd "C-c z") (make-sparse-keymap))
(global-set-key (kbd "C-c z <up>") 'ielm-up)
(global-set-key (kbd "C-c z <down>") 'ielm-down)
(global-set-key (kbd "C-c z <left>") 'ielm-left)
(global-set-key (kbd "C-c z <right>") 'ielm-right)

(global-set-key (kbd "C-x 4 /") 'winner-undo)
(global-set-key (kbd "C-x 4 ?") 'winner-redo)

(global-set-key (kbd "C-x M-l") 'insert-line-number)

(global-set-key (kbd "C-x t") 'konsole-tailf)
(global-set-key (kbd "C-x c") 'konsole-cd)
(global-set-key (kbd "C-x G") 'konsole-tig-file)
(global-set-key (kbd "C-x I") 'imv-open)

(global-set-key (kbd "C-x F") 'findgrep)

(global-set-key (kbd "C-c C") 'to-camel-case-at-point)

(global-set-key (kbd "C-c C-v")
                (let ((m (make-sparse-keymap)))
                  (define-key m (kbd "d") 'insert-current-date)
                  m))

(global-set-key (kbd "C-x B") 'decode-base64)
