(load "server")
(unless (server-running-p)
  (server-start))

(setq kec:config-dir
      (file-name-directory load-file-name))

(dolist (path '(""  
                "kiwanami/emacs-window-manager/"
                "elpa/multi-term-20110326.608/"
                "elpa/auto-complete-20131128.233"
                "elpa/yasnippet-20131031.628"
                "elpa/auto-complete-clang-20120612.2224"
                "elpa/popup-20130708.2245"
                "auto-java-complete"))
  (add-to-list 'load-path (concat kec:config-dir path)))

(load "utils/lisp-utils")
(load "utils/general-utils")
(load "utils/windmove-extra")
(load "utils/window-expand")

(load "config/modes")
(load "config/vars")
(load "config/ac")
(load "config/hooks")
(load "config/keymaps")
(load "config/c")
(load "config/python")
(load "config/asm")
;; (load "config/xsel")
(load "config/jde-mode-extra")
(load "config/slime")

