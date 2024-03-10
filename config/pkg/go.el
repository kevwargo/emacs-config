(require 'go-tag)

(defvar-local go-test-verbose nil)

(defun go-toggle-test-verbose ()
  (interactive)
  (let ((new-val (null go-test-verbose)))
    (setq go-test-verbose new-val)
    (message "go-test-verbose set to %S" new-val)))

(defun go-tag-add-json (&optional transform)
  (interactive (list (go-tag--read-transform current-prefix-arg)))
  (let ((go-tag-args (append go-tag-args transform)))
    (go-tag-add "json")))

(defun go-tag-refresh-json (&optional transform)
  (interactive (list (go-tag--read-transform current-prefix-arg)))
  (let ((go-tag-args (append go-tag-args transform)))
    (go-tag-refresh "json")))

(defun go-tag--read-transform (raw)
  (when raw
    (list "-transform"
          (if (stringp raw)
              raw
            (completing-read "Transform (letter case): "
                             '("snakecase"
                               "camelcase"
                               "lispcase"
                               "pascalcase"
                               "keep"))))))

(defun go-lsp-find-implementation ()
  "Find the non-mock implementations of the method at point.

If there is exactly one such implementation,
jump to it immediately without showing the xref buffer."
  (interactive)
  (let* ((lsp-xref-force-references t)
         (xref-callback xref-show-definitions-function)
         (xref-show-definitions-function
          (lambda (fetcher options)
            (let ((xrefs (--remove
                          (s-ends-with? "_mock.go"
                                        (-> it
                                            xref-item-location
                                            xref-file-location-file
                                            file-name-nondirectory))
                          (funcall fetcher))))
              (funcall xref-callback
                       (-const xrefs)
                       (append `((auto-jump . ,(= (length xrefs) 1)))
                               options))))))
    (lsp-find-implementation)))

(defun setup-go-mode ()
  (add-hook 'before-save-hook 'gofmt-before-save 0 t))

(add-hook 'go-mode-hook 'setup-go-mode)

(define-key go-mode-map (kbd "C-{") 'embrace-selected-lines)
(define-key go-mode-map (kbd "C-x V") 'go-toggle-test-verbose)
(define-key go-mode-map (kbd "C-x t") 'go-tag-refresh-json)
(define-key go-mode-map (kbd "C-x T") 'go-tag-add-json)
(define-key go-mode-map (kbd "M-/") 'go-lsp-find-implementation)
