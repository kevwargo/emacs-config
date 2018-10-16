(defun yaml-enable-indent-tools ()
  (indent-tools-minor-mode 1))

(add-hook 'yaml-mode-hook 'yaml-enable-indent-tools)
