(require 'go-tag)

(defvar-local go-test-verbose nil)

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
    (go-tag-refresh "json")))

(defun -go-tag-read-transform (raw)
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

(defun setup-go-mode ()
  (add-hook 'before-save-hook 'gofmt-before-save 0 t)
  (local-set-key (kbd "C-{") 'embrace-selected-lines)
  (local-set-key (kbd "C-x V") 'go-toggle-test-verbose)
  (local-set-key (kbd "C-x t") 'go-tag-refresh-json)
  (local-set-key (kbd "C-x T") 'go-tag-add-json))

(add-hook 'go-mode-hook 'setup-go-mode)
