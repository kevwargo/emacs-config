(defun konsole-tailf (filename)
  (interactive
   (list (ido-read-file-name "Konsole-tailf: ")))
  (shell-command (concat "konsole-tailf "
                         (shell-quote-argument filename)
                         " >/dev/null")))

(defun konsole-cd (dirname)
  (interactive
   (list (ido-read-directory-name "Konsole-cd: ")))
  (shell-command (concat "konsole-cd "
                         (shell-quote-argument dirname)
                         " >/dev/null")))

(defun konsole-tig-file (filename)
  (interactive
   (list
    (ido-read-file-name "TIG: " (buffer-working-directory) (buffer-file-name))))
  (shell-command (concat "konsole-tig "
                         (shell-quote-argument filename)
                         " >/dev/null")))
