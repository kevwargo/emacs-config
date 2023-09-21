
;; NEW: TODO

(transient-define-prefix findgrep-new ()
  ["Opts"
   ;; :class transient-subgroups
   :setup-children findgrep--setup-children
   ["Global"
    ("-v" "Verbose" ("-v" "--verbose"))
    ("-g" "Globally" ("-g" "--global"))]
   ["Local"
    ("-i" "Include" ("-i" "--include"))
    ("-r" "Reset" ("-r" "--reset"))]
   ]
  ["Execute"
   ([RET] "Run" findgrep-new-run)])

(defun findgrep--setup-children (children)
  (message "%S" children)
  children)

(defun findgrep-new-run ()
  (interactive)
  (message "findgrep-new-run: %S" (--keep (and (transient-switch-p it) (transient-infix-value it)) transient-current-suffixes)))
