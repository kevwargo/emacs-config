(require 's)
(require 'lsp)
(require 'pylint)
(require 'python-black)
(require 'python-isort)

(defvar-local py-venv-path nil)

(defun pipfile-dir ()
  (and (buffer-file-name)
       (locate-dominating-file buffer-file-name "Pipfile")))

(defun py-get-venv ()
  (or py-venv-path
      (setq py-venv-path
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
                      (s-trim output))))))))

(defun setup-py-mode ()
  (message "setup-py-mode called")
  (if (py-get-venv)
      (setq-local pylint-command "pipenv run pylint"))
  (setq-local auto-fill-function nil)
  (setq-local forward-sexp-function nil)
  (setq-local pylint-options
              (append pylint-options
                      '("--disable=missing-docstring"
                        "--disable=wrong-import-order"
                        "--disable=too-many-arguments"
                        "--disable=line-too-long"
                        "--disable=invalid-name"
                        "--disable=bare-except"
                        "--disable=too-many-ancestors")))
                                        ; black should run after isort
  (add-hook 'before-save-hook 'python-isort-buffer -1 t)
  (add-hook 'before-save-hook 'python-black-buffer 0 t)
  (local-set-key (kbd "C-c v")
                 (let ((m (make-sparse-keymap)))
                   (define-key m (kbd "RET") 'py-find-in-venv)
                   (define-key m (kbd "<left>") 'py-find-in-venv-left)
                   (define-key m (kbd "<right>") 'py-find-in-venv-right)
                   (define-key m (kbd "<up>") 'py-find-in-venv-up)
                   (define-key m (kbd "<down>") 'py-find-in-venv-down)
                   m))
  (lsp))

(defun pylint-dir (dir)
  (interactive (list (ido-read-directory-name "Pylint whole dir: ")))
  (let ((command (mapconcat
                  'identity
                  (append `(,pylint-command) pylint-options `(,dir))
                  " ")))
    (compilation-start command 'pylint-mode)))

(defun pylint-pipenv ()
  (interactive)
  (let ((pylint-command "pipenv")
        (pylint-options (append '("run" "pylint") pylint-options)))
    (pylint)))

(defun py-get-venv-path ()
  (let* ((py-venv-path (py-get-venv))
         (site-packages-template (and py-venv-path
                                      (concat py-venv-path "/lib/python*/site-packages")))
         (site-packages (and site-packages-template
                             (car-safe (file-expand-wildcards site-packages-template)))))
    (or site-packages
        py-venv-path)))

(defun py-find-in-venv ()
  (interactive)
  (let ((default-directory (or (py-get-venv-path)
                               (buffer-working-directory))))
    (ido-find-file)))

(defun py-find-in-venv-dir (dir)
  (let ((cwd (or (py-get-venv-path)
                 (buffer-working-directory))))
    (windmove-do-window-select dir)
    (let ((default-directory cwd))
      (ido-find-file))))

(defun py-find-in-venv-left ()
  (interactive)
  (py-find-in-venv-dir 'left))

(defun py-find-in-venv-right ()
  (interactive)
  (py-find-in-venv-dir 'right))

(defun py-find-in-venv-up ()
  (interactive)
  (py-find-in-venv-dir 'up))

(defun py-find-in-venv-down ()
  (interactive)
  (py-find-in-venv-dir 'down))

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
                 ((stringp form)
                  (py--mark-base form))
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
  (async-shell-command
   (format "pip install --user %s"
           (shell-quote-argument
            (file-truename
             (or
              (locate-dominating-file default-directory "setup.py")
              (error "Directory %s does not belong to a pip project" default-directory)))))))

(add-hook 'python-mode-hook 'setup-py-mode)
