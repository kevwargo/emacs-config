(require 'project)
(require 'lsp-mode)

(defvar lsp-modes
  '((go-mode . "go.mod")
    (python-mode . ("Pipfile" "setup.py"))
    (typescript-mode . "package.json")
    (java-mode . find-custom-lsp-java-project)))

(defun setup-lsp-mode ()
  (when-let ((file-name (buffer-file-name))
             (project-descriptor (cdr-safe (assoc major-mode lsp-modes))))
    (cl-labels ((find-prj-dir (dir desc)
                              (cond ((consp desc)
                                     (or (locate-dominating-file dir (car desc))
                                         (find-prj-dir dir (cdr desc))))
                                    ((stringp desc)
                                     (find-prj-dir dir (list desc))))))
      (let ((project-find-functions (append
                                     (list
                                      (lambda (dir)
                                        (if-let ((prj-dir (find-prj-dir dir project-descriptor)))
                                            (progn
                                              (message "Found LSP project dir for %s (%s)" file-name major-mode)
                                              (cons 'vc prj-dir))
                                          (message "LSP project dir for %s (%s) not found" file-name major-mode)
                                          nil)))
                                     project-find-functions)))
        (lsp)))))

(add-hook 'after-change-major-mode-hook 'setup-lsp-mode)
(define-key lsp-mode-map (kbd "M-e") #'lsp-execute-code-action)
(define-key lsp-mode-map (kbd "C-c C-z") #'lsp-organize-imports)
(define-key lsp-mode-map (kbd "C-x w") #'lsp-rename)
