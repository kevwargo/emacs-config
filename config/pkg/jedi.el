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

(add-hook 'python-mode-hook 'jedi-setup)

(setq jedi:complete-on-dot t)
