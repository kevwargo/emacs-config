(defun konsole-cd (dirname)
  (interactive
   (list (ido-read-directory-name "Konsole-cd: ")))
  (start-process "konsole-cd" nil "konsole-cd" dirname))

(keymap-global-set "C-x c" 'konsole-cd)
