(package-initialize)

(load "server")
(unless (server-running-p)
  (server-start))
(setq server-log t)

(setq kec:config-dir
      (file-name-directory (file-truename load-file-name)))


(add-to-list 'load-path kec:config-dir)

(load "config/custom")

(load "utils/lisp-utils")
(load "utils/general-utils")
(load "utils/convert")
(load "utils/windmove-extra")
(load "utils/term")
(load "utils/window-expand")
(load "utils/isearch-sexp")
(load "utils/konsole")
(load "utils/findgrep")
(load "utils/sort-imports")

(load "config/modes")
(load "config/vars")
(load "config/faces")
(load "config/hooks")
(load "config/keymaps")
(load "config/c")
(load "config/asm")
(load "config/slime")
(load "config/comint")
(load "config/js")
(load "config/makefile")
(load "config/sh")
(load "config/debug")
(load "config/after-init")
