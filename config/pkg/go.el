(require 'dash)
(require 'go-tag)
(require 'lsp-mode)
(require 's)

(defvar-local go-test-verbose nil)

(defun go-toggle-test-verbose ()
  (interactive)
  (let ((new-val (null go-test-verbose)))
    (setq go-test-verbose new-val)
    (message "go-test-verbose set to %S" new-val)))

(defmacro go-test--toggle-arg (arg &optional matcher)
  `(let* ((all (and go-test-args (s-split " " go-test-args)))
          (others (--remove ,(or matcher `(equal it ,arg)) all)))
     (setq all
           (if (equal others all)
               (cons ,arg all)
             others))
     (message "go-test-args set locally to %S"
              (setq-local go-test-args
                          (and all (s-join " " all))))))

(defun go-toggle-test-race ()
  (interactive)
  (go-test--toggle-arg "-race" (member it '("-race" "--race"))))

(defun go-toggle-test-cache ()
  (interactive)
  (go-test--toggle-arg "-count=1" (s-starts-with? "-count" it)))

(defun go-test-enable-line-wrapping ()
  (setq-local truncate-lines nil))

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

(defcustom gofumpt-cmd "gofumpt" "" :group 'gofumpt+gci)
(defcustom gofumpt-args nil "" :group 'gofumpt+gci)

(defcustom gci-goimports-cmd "goimports" "" :group 'gofumpt+gci)
(defcustom gci-cmd "gci" "" :group 'gofumpt+gci)
(defcustom gci-subcommand "print" "" :group 'gofumpt+gci)
(defcustom gci-sections '("Standard" "Default") "" :group 'gofumpt+gci)
(defcustom gci-args nil "" :group 'gofumpt+gci)

(reformatter-define gofumpt+gci
  :program "bash"
  :args (list "-c" (format "%s %s | %s | %s %s %s %s"
                           gofumpt-cmd
                           (mapconcat #'shell-quote-argument gofumpt-args " ")
                           gci-goimports-cmd
                           gci-cmd
                           gci-subcommand
                           (mapconcat (lambda (s)
                                        (format "--section %s" (shell-quote-argument s)))
                                      gci-sections " ")
                           (mapconcat #'shell-quote-argument gci-args " ")))
  :lighter " gofumpt+gci")

(keymap-set go-mode-map "C-{" 'embrace-selected-lines)
(keymap-set go-mode-map "C-x V" 'go-toggle-test-verbose)
(keymap-set go-mode-map "C-x R" 'go-toggle-test-race)
(keymap-set go-mode-map "C-x C" 'go-toggle-test-cache)
(keymap-set go-mode-map "C-x t" 'go-tag-refresh-json)
(keymap-set go-mode-map "C-x T" 'go-tag-add-json)
(keymap-set go-mode-map "M-/" 'go-lsp-find-implementation)

(add-hook 'go-mode-hook 'gofumpt+gci-on-save-mode)
(add-hook 'go-test-mode-hook 'go-test-enable-line-wrapping)
