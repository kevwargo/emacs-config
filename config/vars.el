(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)


(setq auto-mode-alist (append auto-mode-alist '(("/etc/cdnet" . conf-mode)
                                                ("\\.e\\(build\\|class\\)" . sh-mode)
                                                ("\\.pddl" . PDDL-mode))))

(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)
(setq tab-always-indent 'complete)

(setq scroll-step 1)
(setq mouse-wheel-scroll-amount '(3 ((shift) . 3)))
(setq mouse-wheel-progressive-speed nil)

(set-language-environment "utf-8")

(put 'narrow-to-region 'disabled nil)
