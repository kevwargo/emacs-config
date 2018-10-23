(require 's)
(require 'jedi-core)
(require 'uuidgen)

(defun jedi-setup ()
  (let ((venv-path
         (with-temp-buffer
           (if (= (call-process shell-file-name
                                nil
                                (list t nil)
                                nil
                                "-c" "pipenv --venv")
                  0)
               (s-trim (buffer-substring-no-properties (point-min)
                                                       (point-max))))))
        jedi-args)
    (when (and venv-path (file-directory-p venv-path))
      (setq-local jedi:server-args (list
                                    "--virtual-env" venv-path
                                    "--log" (concat kec:config-dir "jedi-logs/" (uuidgen-4) ".log")
                                    "--log-level" "DEBUG"))))
  (jedi:setup))

(add-hook 'python-mode-hook 'jedi-setup)

(setq jedi:complete-on-dot t)
