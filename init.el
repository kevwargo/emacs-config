(load "server")
(unless (server-running-p)
  (server-start))
(setq server-log t)

(require 'package)
(package-initialize)

(setq kec:config-dir
      (file-name-directory (file-truename load-file-name)))


(add-to-list 'load-path kec:config-dir)

(load "utils/lisp-utils")
(load "utils/general-utils")
(load "utils/windmove-extra")
(load "utils/term")
(load "utils/window-expand")
(load "utils/isearch-sexp")

(load "config/modes")
(load "config/vars")
(load "config/faces")
(load "config/custom")
(load "config/hooks")
(load "config/keymaps")
(load "config/c")
(load "config/python")
(load "config/asm")
;; (load "config/jde-mode-extra")
(load "config/slime")
(load "config/comint")
(load "config/term")
(load "config/web")
(load "config/js")
(load "config/after-init")

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'scroll-left 'disabled nil)
