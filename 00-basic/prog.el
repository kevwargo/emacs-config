;; -*- lexical-binding: t -*-

(defun prog-make-build ()
  (interactive)
  (when-let ((prj-root (locate-dominating-file (buffer-file-name) "Makefile")))
    (message "Running 'make build' in %s" prj-root)
    (message "%s" (with-temp-buffer
                    (let ((default-directory prj-root))
                      (call-process "make" nil (list t t) nil "build"))
                    (buffer-string)))))

(keymap-set prog-mode-map "C-c C-c" 'prog-make-build)
