
;;; python-mode site-lisp configuration

(add-to-list 'load-path (concat kec:config-dir "python-mode"))

(load (concat kec:config-dir "python-mode/python-mode.el"))
(autoload 'python-mode "python-mode" "Major mode for editing Python files." t)
(autoload 'jython-mode "python-mode" "Major mode for editing Jython files." t)
(autoload 'py-shell "python-mode"
  "Start an interactive Python interpreter in another window." t)

(add-to-list 'auto-mode-alist '("\\.py$" . python-mode))
(add-to-list 'auto-mode-alist '("\\.pyx$" . python-mode))
(add-to-list 'interpreter-mode-alist '("python" . python-mode))
(add-to-list 'interpreter-mode-alist '("jython" . jython-mode))

