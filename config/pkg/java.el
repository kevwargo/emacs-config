(require 'lsp-java)
(require 'f)

(defconst lsp-java-custom-project-file ".lsp-java-project.el")

(defvar-local google-java-format-jar nil)

(defun find-custom-lsp-java-project (dir)
  (or (when-let* ((prj-desc-dir (locate-dominating-file dir lsp-java-custom-project-file))
                  (prj-desc-file (f-join prj-desc-dir lsp-java-custom-project-file))
                  (prj-desc (car (read-from-string (f-read-text prj-desc-file 'utf-8))))
                  (prj-subdir (alist-get 'project-dir prj-desc))
                  (prj-dir (f-join prj-desc-dir prj-subdir)))
        (if (f-dir? prj-dir)
            (progn
              (message "Using %s as a custom lsp-java project" prj-dir)
              (cons 'vc prj-dir))
          (message "Cannot use %s: not a directory" prj-dir)
          nil))
      (progn
        (message "Custom lsp-java project configuration not found")
        nil)))

(reformatter-define google-java-format
  :program "java"
  :args (if google-java-format-jar
            (list "-jar" google-java-format-jar "--aosp" "-")
          (error "`google-java-format-jar' is not defined"))
  :lighter " GJF")

(add-hook 'java-mode-hook 'lsp)
(add-hook 'java-mode-hook 'google-java-format-on-save-mode)
(add-to-list 'project-find-functions 'find-custom-lsp-java-project)

(define-key lsp-mode-map (kbd "M-e") #'lsp-execute-code-action)
(define-key lsp-mode-map (kbd "C-c C-z") #'lsp-organize-imports)
