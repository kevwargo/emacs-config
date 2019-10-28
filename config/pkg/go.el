(require 'go-guru)

(defvar-local go-test-verbose nil)

(defun go-dep-gopath ()
  (let ((d (locate-dominating-file buffer-file-name
                                   (lambda (dir)
                                     (setq dir (or (and (file-directory-p dir) dir)
                                                   (file-name-directory dir)))
                                     ;; (scratch-log "dir: %S" dir)
                                     ;; (let ((files (directory-files dir nil nil t))
                                     ;;       gopkg-toml-found
                                     ;;       gopkg-lock-found
                                     ;;       vendor-found
                                     ;;       src-found)
                                     ;;   (dolist (file files)
                                     ;;     (cond
                                     ;;      ((string= file "Gopkg.toml")
                                     ;;       )
                                     nil))))
    (if d
        (list d))))

(defun godef-smart-jump ()
  (interactive)
  (let ((keys (this-command-keys-vector))
        (process-environment (append
                              (remove-if
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

(defun go-toggle-ac ()
  (interactive)
  (if (member 'ac-source-go ac-sources)
      (progn
        (setq ac-sources (delq 'ac-source-go ac-sources))
        (message "Special autocompletion for Go disabled"))
    (push 'ac-source-go ac-sources)
    (message "Special autocompletion for Go enabled")))

(defun go-toggle-ac-autostart ()
  (interactive)
  (setq-local ac-auto-start (null ac-auto-start))
  (message "Go AutoComplete auto-start %sabled" (if ac-auto-start "en" "dis")))

(defun go-toggle-test-verbose ()
  (interactive)
  (let ((new-val (null go-test-verbose)))
    (setq go-test-verbose new-val)
    (message "go-test-verbose set to %S" new-val)))

(defun go-hook ()
  (local-set-key (kbd "C-c C-f") 'gofmt)
  (local-set-key (kbd "C-c C-c") 'go-goto-map)
  (local-set-key (kbd "M-.") 'godef-smart-jump)
  (local-set-key (kbd "M->") 'godef-smart-jump)
  (local-set-key (kbd "C-x h") 'godoc-at-point)
  (local-set-key (kbd "C-{") 'embrace-selected-lines)
  (local-set-key (kbd "C-c <C-tab>") 'go-toggle-ac-autostart))

(add-hook 'go-mode-hook 'go-hook)
(add-hook 'before-save-hook 'gofmt-before-save)
