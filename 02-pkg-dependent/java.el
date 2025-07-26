(require 'f)
(require 'project)
(require 'lsp-java)

(defconst lsp-java-custom-project-file ".lsp-java-project.el")

(defun find-custom-lsp-java-project (dir)
  (if-let* ((prj-desc-dir (locate-dominating-file dir lsp-java-custom-project-file))
            (prj-desc-file (f-join prj-desc-dir lsp-java-custom-project-file))
            (prj-desc (car (read-from-string (f-read-text prj-desc-file 'utf-8))))
            (prj-subdir (alist-get 'project-dir prj-desc))
            (prj-dir (f-join prj-desc-dir prj-subdir)))
      (if (f-dir? prj-dir)
          (progn
            (message "Using %s as a custom lsp-java project" prj-dir)
            prj-dir)
        (message "Cannot use %s: not a directory" prj-dir)
        nil)
    (message "Custom lsp-java project configuration not found")
    nil))

(defun lsp-java-enable-auto-format ()
  (setq-local lsp-java-format-enabled t)
  (setq-local lsp-java-format-settings-url
              "https://raw.githubusercontent.com/google/styleguide/gh-pages/eclipse-java-google-style.xml")
  (add-hook 'before-save-hook 'lsp-format-buffer nil t)
  (add-hook 'before-save-hook 'lsp-organize-imports nil t))

(add-hook 'java-mode-hook 'lsp-java-enable-auto-format)
