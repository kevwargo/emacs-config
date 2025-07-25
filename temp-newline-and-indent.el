(defun newline-and-indent (&optional arg)
  "Insert a newline, then indent according to major mode.
Indentation is done using the value of `indent-line-function'.
In programming language modes, this is the same as TAB.
In some text modes, where TAB inserts a tab, this command indents to the
column specified by the function `current-left-margin'.

With ARG, perform this action that many times."
  (interactive "*p")
  (delete-horizontal-space t)
  (unless arg
    (setq arg 1))
    (dotimes (_ arg)
      (newline nil t)
      (indent-according-to-mode)))
