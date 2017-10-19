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

