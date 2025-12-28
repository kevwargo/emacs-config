(defun prog-make-build ()
  (interactive)
  (when-let ((prj-root (locate-dominating-file (buffer-file-name) "Makefile")))
    (message "Running 'make build' in %s" prj-root)
    (message "%s" (with-temp-buffer
                    (let ((default-directory prj-root))
                      (shell-command "make build" (current-buffer) (current-buffer)))
                    (buffer-string)))))

(add-hook 'prog-mode-hook
          (lambda ()
            (keymap-local-set "C-c C-c" 'prog-make-build)))
