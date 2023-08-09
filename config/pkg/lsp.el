(require 'project)
(require 'lsp-mode)

(defvar lsp-modes
  '((go-mode . "go.mod")
    (python-mode . "Pipfile")
    (typescript-mode . "package.json")
    (java-mode . find-custom-lsp-java-project)))

(defun setup-lsp-mode ()
  (when-let ((file-name (buffer-file-name))
             (project-descriptor (cdr-safe (assoc major-mode lsp-modes))))
    (let ((project-find-functions (append
                                   (list
                                    (lambda (dir)
                                      (if-let ((prj-dir (locate-dominating-file dir project-descriptor)))
                                          (progn
                                            (message "Found project dir for %s (%s)" file-name major-mode)
                                            (cons 'vc prj-dir))
                                        (message "Project dir for %s (%s) not found" file-name major-mode)
                                        nil)))
                                   project-find-functions)))
      (lsp))))

(add-hook 'after-change-major-mode-hook 'setup-lsp-mode)
(define-key lsp-mode-map (kbd "M-e") #'lsp-execute-code-action)
(define-key lsp-mode-map (kbd "C-c C-z") #'lsp-organize-imports)
