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
(defvar-local lsp--current-highlights-raw nil)

(defun lsp--update-highlights (highlights)
  (setq lsp--current-highlights-raw highlights
        lsp--current-highlights
        (sort (-map (-lambda ((&DocumentHighlight :range (&Range :start (start &as &Position)
                                                                 :end (end &as &Position))))
                      (mapcar 'lsp--position-to-point (list start end)))
                    lsp--current-highlights-raw)
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

(let ((m lsp-mode-map))
  (keymap-set m "C-c C-z" #'lsp-organize-imports)
  (keymap-set m "C-<prior>" #'lsp-previous-highlight)
  (keymap-set m "C-<next>" #'lsp-next-highlight)
  (keymap-set m "C-x w" #'lsp-rename)
  (keymap-set m "C-x R" #'lsp-find-references))

(add-hook 'after-change-major-mode-hook 'setup-lsp-mode)

(setq lsp-disabled-clients '((typescript-mode . graphql-lsp)))

(setq lsp-log-io t
      lsp-log-io-allowlist-methods '("textDocument/definition" "textDocument/implementation" "textDocument/references"))
