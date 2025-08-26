(require 'dash)
(require 's)

(with-eval-after-load 'lsp-pylsp
  (lsp-register-custom-settings '(("pylsp.plugins.jedi.environment" py-detect-pylsp-jedi-env))))

(defvar-local py-venv-path nil)

(defun pipfile-dir ()
  (and (buffer-file-name)
       (locate-dominating-file (buffer-file-name) "Pipfile")))

(defun py-get-venv ()
  (or py-venv-path
      (setq py-venv-path
            (if-let* ((venv-root (locate-dominating-file (buffer-file-name) ".venv"))
                      (venv-dir (expand-file-name ".venv" venv-root))
                      ((file-directory-p venv-dir)))
                venv-dir
              (let ((default-directory (or (pipfile-dir)
                                           default-directory)))
                (with-temp-buffer
                  (let ((code (call-process shell-file-name
                                            nil
                                            '(t nil)
                                            nil
                                            "-c" "pipenv --venv"))
                        (output (buffer-substring-no-properties (point-min)
                                                                (point-max))))
                    (if (zerop code)
                        (s-trim output)))))))))

(defun setup-py-mode ()
  (setq-local forward-sexp-function nil
              lsp-pylsp-plugins-jedi-environment (py-get-venv)
              lsp-use-workspace-root-for-server-default-directory t
              lsp-format-buffer-on-save t)
  (keymap-local-set "C-c v"
                    (let ((m (make-sparse-keymap)))
                      (dolist (k '("RET" "<left>" "<right>" "<up>" "<down>"))
                        (keymap-set m k 'py-find-in-venv))
                      m))
  (keymap-local-set "C-<" 'py-shift-lines-left)
  (keymap-local-set "C->" 'py-shift-lines-right))

(defun py-detect-pylsp-jedi-env ()
  (let ((ws-buffers (-flatten (mapcar 'lsp--workspace-buffers (lsp-workspaces)))))
    (cond
     ((member (current-buffer) ws-buffers)
      (py-get-venv))
     ((null ws-buffers)
      (message "Unable to detect correct pylsp-jedi-environment, current buffer %S seems to be wrong"
               (current-buffer))
      nil)
     (t
      (message "Using %S for detecting pylsp-jedi-environment instead of current %S"
               (car ws-buffers)
               (current-buffer))
      (with-current-buffer (car ws-buffers)
        (py-get-venv))))))

(defun pylsp-verify-environment ()
  (interactive)
  (let ((venv lsp-pylsp-plugins-jedi-environment))
    (lsp-workspace-show-log (car (lsp-workspaces)))
    (goto-char (point-min))
    (re-search-forward (regexp-quote venv))))

(defun py-shift-lines-left ()
  (interactive)
  (cl-destructuring-bind (start end)
      (selected-lines)
    (python-indent-shift-left start end)))

(defun py-shift-lines-right ()
  (interactive)
  (cl-destructuring-bind (start end)
      (selected-lines)
    (python-indent-shift-right start end)))

(defun py-find-in-venv (&optional dir)
  (interactive (let* ((keys (this-command-keys-vector)))
                 (list (aref keys (1- (length keys))))))
  (let ((cwd (let ((venv (py-get-venv)))
               (or (-some->> venv (expand-file-name "lib/python*/site-packages") file-expand-wildcards car)
                   venv
                   (buffer-working-directory)))))
    (if (memq dir '(up down left right))
        (windmove-do-window-select dir))
    (let ((default-directory cwd))
      (ido-find-file))))

(defvar-local pyexecserver-port 35000)

(defun pyexec-port-set (port)
  (interactive "nPyExec port: ")
  (setq pyexecserver-port port)
  (message "pyexec port is now %d" port))

(defun pyexec-port-inc (&optional amount)
  (interactive "p")
  (pyexec-port-set (+ pyexecserver-port (or amount 1))))

(defun pyexec-port-dec (&optional amount)
  (interactive "p")
  (pyexec-port-set (- pyexecserver-port (or amount 1))))

(defun pyexecserver-send (form)
  (eval `(,(if (fboundp 'save-mark-and-excursion)
               'save-mark-and-excursion
             'save-excursion)
          (pyexecserver-send-1 form))))

(defun pyexecserver-send-1 (form)
  (let ((region (cond
                 ((eq form 'lines)
                  (selected-lines t))
                 ((eq form 'buffer)
                  (cons (point-min) (point-max)))
                 ((region-active-p)
                  (cons (region-beginning) (region-end))))))
    (if (consp region)
        (let ((workbuf (current-buffer)))
          (with-temp-buffer
            (if (/= (let ((tempbuf (current-buffer)))
                      (with-current-buffer workbuf
                        (call-process-region (car region) (cdr region)
                                             "pyexecserver"
                                             nil tempbuf nil
                                             "--port" (int-to-string pyexecserver-port)
                                             "--exec-stdin")))
                    0)
                (message "pyexec error: %s" (buffer-substring-no-properties (point-min)
                                                                            (point-max)))
              (message "Python form `%S' executed successfully" form)))))))

(defun pyexec-buffer ()
  (interactive)
  (pyexecserver-send 'buffer))

(defun pyexec-region ()
  (interactive)
  (pyexecserver-send 'region))

(defun pyexec-lines ()
  (interactive)
  (pyexecserver-send 'lines))

(defun pip-install-user-current ()
  (interactive)
  (let* ((setup-py-dir (file-truename
                        (or
                         (locate-dominating-file (or (buffer-file-name) default-directory)
                                                 "setup.py")
                         (user-error "File %s does not belong to a pip project" (buffer-file-name)))))
         (proc-buf (get-buffer-create (format "*pip-install* %s" setup-py-dir)))
         (async-shell-command-display-buffer nil)
         proc)
    (async-shell-command (format "pip install --user %s"
                                 (shell-quote-argument setup-py-dir))
                         proc-buf)
    (when (process-live-p (setq proc (get-buffer-process proc-buf)))
      (set-process-sentinel proc
                            (lambda (proc _)
                              (unless (process-live-p proc)
                                (if (zerop (process-exit-status proc))
                                    (quit-windows-on (process-buffer proc)))))))))

(defun py-init-project-files ()
  (interactive)
  (when-let* ((current-file (buffer-file-name))
              (dir (or (locate-dominating-file current-file "setup.py")
                       (locate-dominating-file current-file ".git")
                       (file-name-directory current-file)))
              (pyproject-file (file-name-concat dir "pyproject.toml"))
              (flake8-file (file-name-concat dir ".flake8")))
    (unless (file-exists-p pyproject-file)
      (write-region "[tool.black]\nline-length=120\n" nil pyproject-file)
      (message "Initialized %s" pyproject-file))
    (unless (file-exists-p flake8-file)
      (write-region "[flake8]\nmax-line-length=120\n" nil flake8-file)
      (message "Initialized %s" flake8-file))))

(defun py-swap-quotes (beg end)
  (interactive "r")
  (save-excursion
    (goto-char beg)
    (while (< (point) end)
      (let ((c (char-after)))
        (cond
         ((eq c ?') (delete-char 1) (insert-char ?\"))
         ((eq c ?\") (delete-char 1) (insert-char ?'))))
      (unless (eobp) (forward-char)))))

(add-hook 'python-mode-hook 'setup-py-mode)
