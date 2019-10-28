(defvar magit-pull-request-last-url-alist nil)

(defun magit-diff-toggle-whitespace ()
  (interactive)
  (let ((predicate (lambda (opt)
                     (or (string= opt "-w")
                         (string= opt "--ignore-all-space"))))
        (args magit-buffer-diff-args))
    (if (find-if predicate args)
        (setq-local magit-buffer-diff-args
                    (remove-if predicate args))
      (setq-local magit-buffer-diff-args
                  (append args '("--ignore-all-space")))))
  (magit-refresh))

(defun magit-custom-keys-hook ()
  (local-set-key (kbd "<C-tab>") 'magit-section-cycle-diffs)
  (local-set-key (kbd "w") 'magit-diff-toggle-whitespace))

(defun magit-handle-pull-request-create (proc string)
  (when (string-match "^remote: *\\(https://bitbucket.org/.*/pull-requests/new[^ ]*\\)" string)
    (let* ((url (match-string-no-properties 1 string))
           (gitdir (magit-gitdir))
           (entry (assoc gitdir magit-pull-request-last-url-alist)))
      (if entry
          (setcdr entry url)
        (setq magit-pull-request-last-url-alist
              (append magit-pull-request-last-url-alist
                      `((,gitdir . ,url))))))))

(defun magit-pull-request-create-post-refresh ()
  (when-let* ((entry (assoc (magit-gitdir) magit-pull-request-last-url-alist))
              (url (cdr entry)))
    (when (y-or-n-p (format "Create pull request (%s)? " url))
      (browse-url url))
    (setcdr entry nil)))

(add-hook 'magit-process-prompt-functions 'magit-handle-pull-request-create)
(add-hook 'magit-post-refresh-hook 'magit-pull-request-create-post-refresh)
(add-hook 'magit-mode-hook 'magit-custom-keys-hook)

(global-set-key (kbd "M-m")
                (let ((m (make-sparse-keymap)))
                  (define-key m (kbd "C") 'magit-commit)
                  (define-key m (kbd "c") 'magit-checkout)
                  (define-key m (kbd "m") 'magit-merge)
                  (define-key m (kbd "l") 'magit-log-all)
                  (define-key m (kbd "d") 'magit-diff)
                  (define-key m (kbd "D") 'magit-diff-unstaged)
                  (define-key m (kbd "w") (lambda ()
                                            (interactive)
                                            (magit-diff-unstaged '("-w"))))
                  (define-key m (kbd "s") 'magit-status)
                  (define-key m (kbd "f") 'magit-find-file)
                  (define-key m (kbd "x") 'magit-reset)
                  m))
