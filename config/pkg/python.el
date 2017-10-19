;; (load "python-mode/site-lisp-python-mode")

(defface py-overloaders-face
  '((t (:inherit font-lock-keyword-face :foreground "DarkBlue")))
  "Face for functions-overloaders: __init__, __new__, etc."
  :group 'python-mode)
(defvar py-overloaders-face 'py-overloaders-face)

;; Editing python highlighting
(defun py-kw-customize-manual ()
  ; Adding highlighting for overloaders (__init__, __new__, __str__, etc.)
  (font-lock-add-keywords nil
                          `((,(rx symbol-start "__"
                                  (or "new"     "init" "del" "repr" "str" "lt"   "le"
                                      "eq"      "ne"   "gt"  "ge"   "cmp" "rcmp" "hash"
                                      "nonzero" "unicode" "getattr" "setattr" "delattr"
                                      "getattribute" "get" "set" "delete" "call" "len"
                                      "getitem" "setitem" "delitem" "iter" "reversed"
                                      "contains" "getslice" "setslice" "delslice"
                                      (group (opt (or "r" "i"))
                                             (or "add" "sub" "mul" "floordiv" "mod"
                                                 "divmod" "pow" "lshift" "rshift"
                                                 "and" "xor" "or" "div" "truediv"))
                                      "neg" "pos" "abs" "invert" "complex" "int" "long"
                                      "float" "oct" "hex" "index" "coerce" "enter" "exit")
                                  "__" symbol-end)
                             . py-overloaders-face))))

(defun py-keymap-customize ()
  (local-set-key (kbd "#") 'self-insert-command)
  (local-set-key (kbd "C-c x") (make-sparse-keymap))
  (local-set-key (kbd "C-c x c") 'py-execute-class)
  (local-set-key (kbd "C-c x d") 'py-execute-def)
  (local-set-key (kbd "C-c x b") 'py-execute-buffer)
  (local-set-key (kbd "C-c x B") 'py-execute-block)
  (local-set-key (kbd "C-c e") (make-sparse-keymap))
  (local-set-key (kbd "C-c e d") 'py-end-of-def-or-class)
  (local-set-key (kbd "C-c e c") 'py-end-of-class)
  (local-set-key (kbd "C-c e l") 'py-end-of-clause)
  (local-set-key (kbd "C-c e f") 'py-end-of-def)
  (local-set-key (kbd "C-c e b") 'py-end-of-block)
  (local-set-key (kbd "C-c e m") 'py-end-of-comment)
  (local-set-key (kbd "C-c e e") 'py-end-of-expression)
  (local-set-key (kbd "C-c e i") 'py-end-of-if-block)
  (local-set-key (kbd "C-c e s") 'py-end-of-string)
  (local-set-key (kbd "C-c e t") 'py-end-of-try-block)
  (local-set-key (kbd "C-c e p") 'py-end-of-top-level)
  (local-set-key (kbd "C-c a") (make-sparse-keymap))
  (local-set-key (kbd "C-c a d") 'py-beginning-of-def-or-class)
  (local-set-key (kbd "C-c a c") 'py-beginning-of-class)
  (local-set-key (kbd "C-c a l") 'py-beginning-of-clause)
  (local-set-key (kbd "C-c a f") 'py-beginning-of-def)
  (local-set-key (kbd "C-c a b") 'py-beginning-of-block)
  (local-set-key (kbd "C-c a m") 'py-beginning-of-comment)
  (local-set-key (kbd "C-c a e") 'py-beginning-of-expression)
  (local-set-key (kbd "C-c a i") 'py-beginning-of-if-block)
  (local-set-key (kbd "C-c a t") 'py-beginning-of-try-block)
  (local-set-key (kbd "C-c a p") 'py-beginning-of-top-level)
  (local-unset-key (kbd "<C-backspace>")))

(defun py-kill-buffer-on-shell-exit ()
  (set-process-sentinel (get-buffer-process py-buffer-name)
                        (lambda (proc state)
                          (when (string-match-p (rx (or "finished" "exited"))
                                                state)
                            (kill-buffer (process-buffer proc))))))

(defun py-shell-keymap-modify ()
  (local-set-key (kbd "C-c TAB") 'py-shell-complete))

(setq py-install-directory
      "/home/jarasz/dev/programming/lisp/emacs/emacs-config/python-mode/")

;;; use IPython
;; (setq-default py-shell-name "ipython")
;; (setq-default py-which-bufname "IPython")
; use the wx backend, for both mayavi and matplotlib
;; (setq py-python-command-args
;;   '("--gui=wx" "--pylab=wx" "-colors" "Linux"))
(setq py-force-py-shell-name-p nil)

; switch to the interpreter after executing code
;; (setq py-shell-switch-buffers-on-execute-p t)
(setq py-switch-buffers-on-execute-p nil)
; don't split windows
(setq py-split-windows-on-execute-p nil)
; try to automagically figure out indentation
(setq py-smart-indentation t)


(add-hook 'python-mode-hook 'py-kw-customize-manual)
;; (remove-hook 'python-mode-hook 'py-kw-customize-manual)
(add-hook 'python-mode-hook 'py-keymap-customize)
(add-hook 'py-shell-hook 'py-kill-buffer-on-shell-exit)
(add-hook 'py-shell-hook 'comint-mode-keymap-modify)
(add-hook 'py-shell-hook 'py-shell-keymap-modify)

(define-key python-mode-map [(control backspace)] nil)
