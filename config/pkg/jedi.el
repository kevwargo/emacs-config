(require 's)
(require 'jedi-core)
(require 'uuidgen)

(defun jedi-setup ()
  (let ((py-venv-path (py-get-venv))
        (jedi-args))
    (when (and py-venv-path (file-directory-p py-venv-path))
      (setq-local jedi:server-args (list
                                    "--virtual-env" py-venv-path
                                    ))))
  (jedi:setup))

(defun jedi-goto-other-window ()
  (interactive)
  (jedi:goto-definition t))

(defun jedi-custom-keys ()
  (let ((m jedi-mode-map))
    (define-key m (kbd "M->") 'jedi-goto-other-window)))

(add-hook 'python-mode-hook 'jedi-setup)
(add-hook 'jedi-mode-hook 'jedi-custom-keys)

(setq jedi:complete-on-dot t)
