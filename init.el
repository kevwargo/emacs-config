(require 'server)
(unless (server-running-p)
  (server-start))
(setq server-log t)

(setq kec:config-dir
      (file-name-directory (file-truename load-file-name)))
(add-to-list 'load-path kec:config-dir)

(load "config/custom")

(load "utils/lisp-utils")
(load "utils/log")
(load "utils/general-utils")
(load "utils/insert")
(load "utils/convert")
(load "utils/windmove-extra")
(load "utils/term")
(load "utils/isearch-sexp")
(load "utils/konsole")
(load "utils/easy-window-split")
(load "utils/key-overrides")
(load "utils/ido")
(load "utils/buffer-menu")
(load "utils/diff")
(load "utils/record-keys")
(load "utils/keymaps")
(load "utils/go-mod")
(load "utils/recent-keys")

(load "config/modes")
(load "config/lisp-modes")
(load "config/vars")
(load "config/faces")
(load "config/hooks")
(load "config/keymaps")
(load "config/c")
(load "config/comint")
(load "config/ielm")
(load "config/ido")
(load "config/js")
(load "config/makefile")
(load "config/sh")
(load "config/debug")
(load "config/temp-files")
(load "config/smerge")
(load "config/flymake")
(load "config/after-init")
