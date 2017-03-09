(require 'eclim)
(require 'eclimd)

(custom-set-variables
 '(eclim-eclipse-dirs '("/opt/eclipse"))
 '(eclim-executable "/opt/eclipse/eclim")
 '(eclimd-executable "/opt/eclipse/eclimd"))

(require 'ac-emacs-eclim-source)
(ac-emacs-eclim-config)

(global-eclim-mode)
