(require 'cl-seq)
(require 'go-tag)

(defvar-local go-test-verbose nil)

(defun godef-smart-jump ()
  (interactive)
  (let ((keys (this-command-keys-vector))
        (process-environment (append
                              (cl-remove-if
                               (lambda (e)
                                 (or (string= e "GOPATH")
                                     (and (> (length e) 7)
                                          (string= (substring e 0 7)
                                                   "GOPATH="))))
                               process-environment)
                              (list (concat "GOPATH="
                                            (or (go-guess-gopath)
                                                (go-original-gopath)))))))
    (cond
      ((equal keys (kbd "M-."))
       (godef-jump (point)))
      ((equal keys (kbd "M->"))
       (godef-jump-other-window (point))))))

(defun go-toggle-test-verbose ()
  (interactive)
  (let ((new-val (null go-test-verbose)))
    (setq go-test-verbose new-val)
    (message "go-test-verbose set to %S" new-val)))

(defun go-tag-add-json (&optional transform)
  (interactive (list (-go-tag-read-transform current-prefix-arg)))
  (let ((go-tag-args (append go-tag-args transform)))
    (go-tag-add "json")))

(defun go-tag-refresh-json (&optional transform)
  (interactive (list (-go-tag-read-transform current-prefix-arg)))
  (let ((go-tag-args (append go-tag-args transform)))
    (scratch-log-expr go-tag-args)
    (go-tag-refresh "json")))

(defun -go-tag-read-transform (raw)
  (when raw
    (list "-transform"
          (if (stringp raw)
              raw
            (read-string "Transform (letter case): " nil nil
                         '("snakecase"
                           "camelcase"
                           "lispcase"
                           "pascalcase"
                           "keep"))))))

(defun golangci-lint (dir)
  (interactive
   (list (ido-read-directory-name
          "Run golangci-lint in: "
          default-directory)))
  (let ((default-directory dir))
    (compilation-start "golangci-lint run --disable-all -E golint ./...")))

(defun golangci-lint-temp (dir)
  (interactive
   (list (ido-read-directory-name
          "Run golangci-lint in: "
          default-directory)))
  (let ((default-directory dir))
    (compilation-start "git diff master | golangci-lint run --out-format colored-line-number --new-from-patch /dev/stdin ./core/... | sort -V")))


(defun go-hook ()
  (local-set-key (kbd "C-c C-f") 'gofmt)
  (local-set-key (kbd "C-c C-c") 'go-goto-map)
  (local-set-key (kbd "M-.") 'godef-smart-jump)
  (local-set-key (kbd "M->") 'godef-smart-jump)
  (local-set-key (kbd "C-x h") 'godoc-at-point)
  (local-set-key (kbd "C-{") 'embrace-selected-lines)
  (local-set-key (kbd "C-x t") 'go-tag-refresh-json)
  (local-set-key (kbd "C-x T") 'go-tag-add-json))

(add-hook 'go-mode-hook 'go-hook)
(add-hook 'before-save-hook 'gofmt-before-save)
