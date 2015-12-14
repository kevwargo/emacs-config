(require 'eclim)
(require 'eclimd)

(custom-set-variables
 '(eclim-eclipse-dirs '("/opt/eclipse-sdk-bin-4.3" "~/.eclipse"))
 '(eclim-executable "~/.eclipse/plugins/org.eclim_2.5.0.21-gca00af8/bin/eclim"))

(require 'ac-emacs-eclim-source)
(ac-emacs-eclim-config)

(global-eclim-mode)
