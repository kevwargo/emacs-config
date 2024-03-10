(require 'cl-seq)
(require 'magit)

(defvar magit-pull-request-last-url-alist nil)
(defvar magit-pull-request-regex-list
  '("^remote: *\\(https://bitbucket.org/.*/pull-requests/\\(new\\|[0-9]+\\)[^ ]*\\)"
    "^remote: *\\(https://github.com/.*/pull/\\(new\\|[0-9]+\\)[^ ]*\\)"))

(defun magit-diff-toggle-whitespace ()
  (interactive)
  (let* ((ignore-all-space? (lambda (opt)
                              (member opt '("-w" "--ignore-all-space"))))
         (previously-enabled (cl-find-if ignore-all-space? magit-buffer-diff-args)))
    (if previously-enabled
        (setq-local magit-buffer-diff-args
                    (cl-remove-if ignore-all-space? magit-buffer-diff-args))
      (setq-local magit-buffer-diff-args
                  (append magit-buffer-diff-args '("--ignore-all-space"))))
    (message "Magit-Diff --ignore-all-space %sabled" (if previously-enabled "dis" "en")))
  (magit-refresh))

(defun magit-custom-keys-hook ()
  (local-set-key (kbd "<C-tab>") 'magit-section-cycle-diffs)
  (local-set-key (kbd "w") 'magit-diff-toggle-whitespace))

(defun magit-delete-file ()
  (interactive)
  (let ((magit-delete-by-moving-to-trash nil))
    (magit-discard)))

(defun magit-status-custom-keys-hook ()
  (local-set-key (kbd "K") 'magit-delete-file))

(defun magit-stashes-custom-keys-hook ()
  (local-set-key (kbd "p") 'magit-stash-pop))

(defun magit-handle-pull-request-create (proc string)
  (when (cl-find-if (lambda (re) (string-match re string))
                 magit-pull-request-regex-list)
    (let* ((url (match-string-no-properties 1 string))
           (gitdir (magit-gitdir))
           (entry (assoc gitdir magit-pull-request-last-url-alist))
           (url-entry (cons url (string= "new" (match-string-no-properties 2 string)))))
      (if entry
          (setcdr entry url-entry)
        (setq magit-pull-request-last-url-alist
              (append magit-pull-request-last-url-alist
                      `((,gitdir . ,url-entry))))))))

(defun magit-pull-request-create-post-refresh ()
  (when-let* ((entry (assoc (magit-gitdir) magit-pull-request-last-url-alist))
              (url-entry (cdr entry)))
    (when (y-or-n-p (format "%s pull request (%s)? "
                            (if (cdr url-entry) "Create" "View")
                            (car url-entry)))
      (browse-url (car url-entry)))
    (setcdr entry nil)))

(defun magit-painted-tag-at-point ()
  (if (eq (get-text-property (magit-point) 'font-lock-face) 'magit-tag)
      (magit-thing-at-point 'git-revision)))

(transient-define-suffix magit-push-current-tag ()
  :if #'magit-painted-tag-at-point
  :description (lambda () (format "Tag %s"
                                  (magit--propertize-face (magit-painted-tag-at-point)
                                                          'magit-tag)))
  (interactive)
  (when-let ((tag (magit-painted-tag-at-point))
             (remote (magit-read-remote (format "Push %s to remote" tag) nil t)))
    (run-hooks 'magit-credential-hook)
    (magit-run-git-async "push" remote tag)))

(transient-append-suffix 'magit-push '(2 1 0) '("t" magit-push-current-tag))

(defun magit-get-default-remote ()
  (let ((remotes (magit-list-remotes)))
    (and (length= remotes 1) (car remotes))))

(defun magit-get-current-branch-remote ()
  (when-let ((branch (magit-get-current-branch))
             (remote (magit-get-default-remote)))
    (cons branch remote)))

(defun magit-push-current-branch-description ()
  (cl-destructuring-bind (branch . remote)
      (magit-get-current-branch-remote)
    (magit--propertize-face (format "%s/%s" remote branch)
                            'magit-branch-remote)))

(transient-define-suffix magit-push-current-branch-to-upstream ()
  :if #'magit-get-current-branch-remote
  :description #'magit-push-current-branch-description
  (interactive)
  (cl-destructuring-bind (branch . remote)
      (magit-get-current-branch-remote)
    (magit-run-git-async "push" "-v" "--set-upstream" remote (format "%s:refs/heads/%s" branch branch))))

(transient-append-suffix 'magit-push '(1 0) '("U" magit-push-current-branch-to-upstream))

(defun magit-get-default-branch-remote ()
  (when-let ((branch (magit-main-branch))
             (remote (magit-get-default-remote)))
    (cons branch remote)))

(defun magit-pull-main-branch-description ()
  (cl-destructuring-bind (branch . remote)
      (magit-get-default-branch-remote)
    (format "Fetch %s and reset %s to %s"
            (magit--propertize-face remote 'magit-branch-remote)
            (magit--propertize-face branch 'magit-branch-local)
            (magit--propertize-face (format "%s/%s" remote branch) 'magit-branch-remote-head))))

(transient-define-suffix magit-pull-main-branch (args)
  :if #'magit-get-default-remote
  :description #'magit-pull-main-branch-description
  (interactive (list (magit-fetch-arguments)))
  (cl-destructuring-bind (branch . remote)
      (magit-get-default-branch-remote)
    (run-hooks 'magit-credential-hook)
    (magit-run-git "fetch" remote args)
    (magit-branch-reset branch (format "%s/%s" remote branch))))

(transient-append-suffix 'magit-fetch '(1 0) '("M" magit-pull-main-branch))

(transient-define-suffix magit-reset-branch-to-current-commit (branch)
  :if #'magit-commit-at-point
  :description (lambda () (concat "reset to " (magit-commit-at-point)))
  (interactive (list (magit-read-local-branch (format "Reset branch to %s" (magit-commit-at-point))
                                              (magit-get-current-branch))))
  (magit-branch-reset branch (magit-commit-at-point)))

(transient-append-suffix 'magit-branch '(2 4 0) '("X" magit-reset-branch-to-current-commit))

(add-hook 'magit-process-prompt-functions 'magit-handle-pull-request-create)
(add-hook 'magit-post-refresh-hook 'magit-pull-request-create-post-refresh)
(add-hook 'magit-mode-hook 'magit-custom-keys-hook)
(add-hook 'magit-status-mode-hook 'magit-status-custom-keys-hook)
(add-hook 'magit-stashes-mode-hook 'magit-stashes-custom-keys-hook)

(global-set-key (kbd "M-m")
                (let ((m (make-sparse-keymap)))
                  (define-key m (kbd "C") 'magit-commit)
                  (define-key m (kbd "c") 'magit-checkout)
                  (define-key m (kbd "b") 'magit-blame-addition)
                  (define-key m (kbd "B") 'magit-blame-echo)
                  (define-key m (kbd "m") 'magit-merge)
                  (define-key m (kbd "l") 'magit-log-all-branches)
                  (define-key m (kbd "d") 'magit-diff)
                  (define-key m (kbd "D") 'magit-diff-unstaged)
                  (define-key m (kbd "w") (lambda ()
                                            (interactive)
                                            (magit-diff-unstaged '("-w"))))
                  (define-key m (kbd "s") 'magit-status)
                  (define-key m (kbd "f") 'magit-find-file)
                  (define-key m (kbd "x") 'magit-reset)
                  m))

(put 'magit-clean 'disabled nil)
