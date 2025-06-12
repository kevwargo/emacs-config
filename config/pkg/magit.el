;; -*- lexical-binding: t; -*-

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

(transient-replace-suffix 'magit-push "t" '("t" magit-push-current-tag))

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

(transient-replace-suffix 'magit-push "u" '("u" magit-push-current-branch-to-upstream))

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
  :if #'magit-get-default-branch-remote
  :description #'magit-pull-main-branch-description
  (interactive (list (magit-fetch-arguments)))
  (cl-destructuring-bind (branch . remote)
      (magit-get-default-branch-remote)
    (let* ((fetch-process (magit-git-fetch remote args))
           (fetch-sentinel (process-sentinel fetch-process)))
      (set-process-sentinel fetch-process
                            (lambda (p event)
                              (prog1
                                  (funcall fetch-sentinel p event)
                                (when (string-match-p "^finished" event)
                                  (magit-branch-reset branch (format "%s/%s" remote branch)))))))))

(transient-append-suffix 'magit-fetch 'magit-fetch-from-upstream '("M" magit-pull-main-branch))

(transient-define-suffix magit-reset-branch-to-current-commit (branch)
  :if #'magit-commit-at-point
  :description (lambda () (concat "reset to " (magit-commit-at-point)))
  (interactive (list (magit-read-local-branch (format "Reset branch to %s" (magit-commit-at-point))
                                              (magit-get-current-branch))))
  (magit-branch-reset branch (magit-commit-at-point)))

(transient-append-suffix 'magit-branch "x" '("X" magit-reset-branch-to-current-commit))

(defun magit-github-open ()
  (interactive)
  (when-let ((url (magit--github-build-url)))
    (browse-url url)))

(defun magit-github-copy ()
  (interactive)
  (when-let ((url (magit--github-build-url)))
    (kill-new url)))

(defun magit--github-build-url ()
  (when-let* ((remote (magit-get-some-remote))
              (remote-url (magit-git-string "remote" "get-url" remote))
              (base-url (and (string-match (rx "github.com" (any ":/")
                                               (group (+ (not (any ":/")))) ; repo owner
                                               "/"
                                               (group (+ (not (any ":/")))) ; repo name
                                               )
                                           remote-url)
                             (format "https://github.com/%s/%s"
                                     (match-string 1 remote-url)
                                     (s-chop-suffix ".git" (match-string 2 remote-url)))))
              (file-name (s-chop-prefix (magit-toplevel)
                                        (or magit-buffer-file-name (buffer-file-name))))
              (rev (if magit-blame-mode
                       (magit-branch-or-commit-at-point)
                     (magit-git-string "rev-parse" (or magit-buffer-revision "HEAD"))))
              (line-range (if (and (region-active-p) (null magit-blame-mode))
                              (format "L%d-L%d"
                                      (line-number-at-pos (region-beginning) t)
                                      (line-number-at-pos (region-end) t))
                            (format "L%d" (line-number-at-pos nil t)))))
    (format "%s/blob/%s/%s#%s" base-url rev file-name line-range)))

(add-hook 'magit-process-prompt-functions 'magit-handle-pull-request-create)
(add-hook 'magit-post-refresh-hook 'magit-pull-request-create-post-refresh)
(add-hook 'magit-mode-hook 'magit-custom-keys-hook)
(add-hook 'magit-status-mode-hook 'magit-status-custom-keys-hook)
(add-hook 'magit-stashes-mode-hook 'magit-stashes-custom-keys-hook)

(let ((m (make-sparse-keymap)))
  (keymap-set m "C" 'magit-commit)
  (keymap-set m "c" 'magit-checkout)
  (keymap-set m "b" 'magit-blame-addition)
  (keymap-set m "B" 'magit-blame-echo)
  (keymap-set m "m" 'magit-merge)
  (keymap-set m "l" 'magit-log-all-branches)
  (keymap-set m "d" 'magit-diff)
  (keymap-set m "D" 'magit-diff-unstaged)
  (keymap-set m "w" (lambda ()
                      (interactive)
                      (magit-diff-unstaged '("-w"))))
  (keymap-set m "s" 'magit-status)
  (keymap-set m "f" 'magit-find-file)
  (keymap-set m "x" 'magit-reset)
  (keymap-set m "g" 'magit-github-open)
  (keymap-set m "G" 'magit-github-copy)
  (keymap-global-set "M-m" m))

(put 'magit-clean 'disabled nil)
