(require 'ivy)
(require 'ivy-xref)

(setq ivy-wrap t)
(setq ivy-xref-use-file-path t)

(setq xref-show-xrefs-function 'ivy-xref-show-xrefs)
(setq xref-show-definitions-function 'ivy-xref-show-defs)
