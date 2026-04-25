(defun konsole-cd (dirname)
  (interactive
   (list (ido-read-directory-name "Konsole-cd: ")))
  (if-let* ((konsctl (executable-find "konsctl")))
      (call-process konsctl nil nil nil "cd" dirname)
    (start-process "konsole-cd" nil "konsole-cd" dirname)))

(keymap-global-set "C-x c" 'konsole-cd)
