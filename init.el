
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
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
(load "utils/windmove-extra")
(load "utils/term")
(load "utils/window-expand")
(load "utils/isearch-sexp")
(load "utils/konsole")

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
(load "config/after-init")
