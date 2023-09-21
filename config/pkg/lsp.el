(require 'lsp-mode)
(require 'project)

(defvar lsp-modes
  '((go-mode . "go.mod")
    (python-mode . ("Pipfile" "setup.py"))
    (typescript-mode . "package.json")
    (js-mode . "package.json")
    (rjsx-mode . "package.json")
    (java-mode . find-custom-lsp-java-project)))

(defvar-local lsp--current-highlights nil)

(defun lsp--update-highlights (highlights)
  (setq lsp--current-highlights
        (sort (-map (-lambda ((&DocumentHighlight :range (&Range :start (start &as &Position)
                                                                 :end (end &as &Position))))
                      (mapcar 'lsp--position-to-point (list start end)))
                    highlights)
              (lambda (h1 h2) (< (car h1) (car h2))))))

(advice-add 'lsp--document-highlight-callback :before 'lsp--update-highlights)

(defun lsp-next-highlight ()
  (interactive)
  (when-let ((highlight (or (--find (> (car it) (point)) lsp--current-highlights)
                            (car lsp--current-highlights))))
    (goto-char (car highlight))))

(defun lsp-previous-highlight ()
  (interactive)
  (when-let ((highlight (or (--last (< (cadr it) (point)) lsp--current-highlights)
                            (car (last lsp--current-highlights)))))
    (goto-char (car highlight))))

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
                                              `(transient . ,prj-dir))
                                          (message "LSP project dir for %s (%s) not found" file-name major-mode)
                                          nil)))
                                     project-find-functions)))
        (lsp)))))

(add-hook 'after-change-major-mode-hook 'setup-lsp-mode)

(let ((m lsp-mode-map))
  (define-key m (kbd "M-e") #'lsp-execute-code-action)
  (define-key m (kbd "C-c C-z") #'lsp-organize-imports)
  (define-key m (kbd "C-<prior>") #'lsp-previous-highlight)
  (define-key m (kbd "C-<next>") #'lsp-next-highlight)
  (define-key m (kbd "C-x w") #'lsp-rename))
