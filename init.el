(load "server")
(unless (server-running-p)
  (server-start))

;; (package-initialize)

(setq kec:config-dir
      (file-name-directory load-file-name))

(dolist (path '(""  
                "kiwanami/emacs-window-manager/"
                "elpa/multi-term-/"
                "elpa/auto-complete-20131128.233"
                "elpa/yasnippet-20131031.628"
                "elpa/auto-complete-clang-20120612.2224"
                "elpa/popup-20130708.2245"
                "auto-java-complete"))
  (add-to-list 'load-path (concat kec:config-dir path)))

;; (add-to-list 'load-path kec:config-dir)

(load "utils/lisp-utils")
(load "utils/general-utils")
(load "utils/windmove-extra")
(load "utils/window-expand")

(load "config/modes")
(load "config/vars")
(load "config/faces.el")
(load "config/hooks")
(load "config/keymaps")
(load "config/c")
(load "config/python")
(load "config/asm")
;; (load "config/xsel")
(load "config/jde-mode-extra")
(load "config/slime")
(load "config/ac")

