(require 'python-mode)
(require 'pylint)

(defvar-local py-venv-path nil)

;; (load "python-mode/site-lisp-python-mode")

;; (defface py-overloaders-face
;;   '((t (:inherit font-lock-keyword-face :foreground "DarkBlue")))
;;   "Face for functions-overloaders: __init__, __new__, etc."
;;   :group 'python-mode)
;; (defvar py-overloaders-face 'py-overloaders-face)

;; ;; Editing python highlighting
;; (defun py-kw-customize-manual ()
;;   ; Adding highlighting for overloaders (__init__, __new__, __str__, etc.)
;;   (font-lock-add-keywords nil
;;                           `((,(rx symbol-start "__"
;;                                   (or "new"     "init" "del" "repr" "str" "lt"   "le"
;;                                       "eq"      "ne"   "gt"  "ge"   "cmp" "rcmp" "hash"
;;                                       "nonzero" "unicode" "getattr" "setattr" "delattr"
;;                                       "getattribute" "get" "set" "delete" "call" "len"
;;                                       "getitem" "setitem" "delitem" "iter" "reversed"
;;                                       "contains" "getslice" "setslice" "delslice"
;;                                       (group (opt (or "r" "i"))
;;                                              (or "add" "sub" "mul" "floordiv" "mod"
;;                                                  "divmod" "pow" "lshift" "rshift"
;;                                                  "and" "xor" "or" "div" "truediv"))
;;                                       "neg" "pos" "abs" "invert" "complex" "int" "long"
;;                                       "float" "oct" "hex" "index" "coerce" "enter" "exit")
;;                                   "__" symbol-end)
;;                              . py-overloaders-face))))

(defun py-shift-lines-left ()
  (interactive)
  (apply 'py-shift-left 1 (selected-lines)))

(defun py-shift-lines-right ()
  (interactive)
  (apply 'py-shift-right 1 (selected-lines)))

(defun py-get-venv ()
  (or py-venv-path
      (setq py-venv-path
            (let ((default-directory (or (and (buffer-file-name)
                                              (locate-dominating-file (buffer-file-name) "Pipfile"))
                                         default-directory)))
              (with-temp-buffer
                (let ((code (call-process shell-file-name
                                          nil
                                          '(t nil)
                                          nil
                                          "-c" "pipenv --venv"))
                      (output (buffer-substring-no-properties (point-min)
                                                              (point-max))))
                  (if (zerop code)
                      (s-trim output))))))))


(defun py-set-local-keys ()
  (local-set-key (kbd "#") 'self-insert-command)
  (local-set-key (kbd "C-c x") (make-sparse-keymap))
  (local-set-key (kbd "C-c x c") 'pyexec-class)
  (local-set-key (kbd "C-c x d") 'pyexec-def)
  (local-set-key (kbd "C-c x b") 'pyexec-buffer)
  (local-set-key (kbd "C-c x B") 'pyexec-block)
  (local-set-key (kbd "C-c x r") 'pyexec-region)
  (local-set-key (kbd "C-c x l") 'pyexec-lines)
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
  (local-set-key (kbd "C-<") 'py-shift-lines-left)
  (local-set-key (kbd "C->") 'py-shift-lines-right)
  (local-set-key (kbd "C-c =") 'pyexec-port-inc)
  (local-set-key (kbd "C-c -") 'pyexec-port-dec)
  (local-set-key (kbd "C-c p") 'pyexec-port-set)
  (local-set-key (kbd "C-c v") (let ((m (make-sparse-keymap)))
                                 (define-key m (kbd "RET") 'py-find-in-venv)
                                 (define-key m (kbd "<left>") 'py-find-in-venv-left)
                                 (define-key m (kbd "<right>") 'py-find-in-venv-right)
                                 (define-key m (kbd "<up>") 'py-find-in-venv-up)
                                 (define-key m (kbd "<down>") 'py-find-in-venv-down)
                                 m))
  (local-unset-key [(control backspace)])
  (local-unset-key (kbd "<C-backspace>"))
  (local-unset-key (kbd "C-c C-d")))

(defun setup-py-mode ()
  (message "setup-py-mode called")
  (py-set-local-keys)
  (if (py-get-venv)
      (setq-local pylint-command "pipenv run pylint"))
  (setq-local auto-fill-function nil)
  (setq-local pylint-options
              (append pylint-options
                      '("--disable=missing-docstring"
                        "--disable=wrong-import-order"
                        "--disable=too-many-arguments"
                        "--disable=line-too-long"
                        "--disable=invalid-name"
                        "--disable=bare-except"
                        "--disable=too-many-ancestors"))))

(defun pylint-dir (dir)
  (interactive (list (ido-read-directory-name "Pylint whole dir: ")))
  (let ((command (mapconcat
                  'identity
                  (append `(,pylint-command) pylint-options `(,dir))
                  " ")))
    (compilation-start command 'pylint-mode)))

(defun py-get-venv-path ()
  (let* ((py-venv-path (py-get-venv))
         (site-packages-template (and py-venv-path
                                      (concat py-venv-path "/lib/python*/site-packages")))
         (site-packages (and site-packages-template
                             (car-safe (file-expand-wildcards site-packages-template)))))
    (or site-packages
        py-venv-path)))

(defun py-find-in-venv ()
  (interactive)
  (let ((default-directory (or (py-get-venv-path)
                               (buffer-working-directory))))
    (ido-find-file)))

(defun py-find-in-venv-dir (dir)
  (let ((cwd (or (py-get-venv-path)
                 (buffer-working-directory))))
    (windmove-do-window-select dir)
    (let ((default-directory cwd))
      (ido-find-file))))

(defun py-find-in-venv-left ()
  (interactive)
  (py-find-in-venv-dir 'left))

(defun py-find-in-venv-right ()
  (interactive)
  (py-find-in-venv-dir 'right))

(defun py-find-in-venv-up ()
  (interactive)
  (py-find-in-venv-dir 'up))

(defun py-find-in-venv-down ()
  (interactive)
  (py-find-in-venv-dir 'down))

(defun py-kill-buffer-on-shell-exit ()
  (set-process-sentinel (get-buffer-process py-buffer-name)
                        (lambda (proc state)
                          (when (string-match-p (rx (or "finished" "exited"))
                                                state)
                            (kill-buffer (process-buffer proc))))))

(defun py-shell-keymap-modify ()
  (local-set-key (kbd "C-c TAB") 'py-shell-complete))

(defvar-local pyexecserver-port 35000)

(defun pyexec-port-set (port)
  (interactive "nPyExec port: ")
  (setq pyexecserver-port port)
  (message "pyexec port is now %d" port))

(defun pyexec-port-inc (&optional amount)
  (interactive "p")
  (pyexec-port-set (+ pyexecserver-port (or amount 1))))

(defun pyexec-port-dec (&optional amount)
  (interactive "p")
  (pyexec-port-set (- pyexecserver-port (or amount 1))))

(defun pyexecserver-send (form)
  (if (fboundp 'save-mark-and-excursion)
      (save-mark-and-excursion
        (pyexecserver-send-1 form))
    (save-excursion
      (pyexecserver-send-1 form))))

(defun pyexecserver-send-1 (form)
  (let ((region (cond
                 ((eq form 'lines)
                  (selected-lines t))
                 ((eq form 'buffer)
                  (cons (point-min) (point-max)))
                 ((stringp form)
                  (py--mark-base form))
                 ((region-active-p)
                  (cons (region-beginning) (region-end))))))
    (if (consp region)
        (let ((workbuf (current-buffer)))
          (with-temp-buffer
            (if (/= (let ((tempbuf (current-buffer)))
                      (with-current-buffer workbuf
                        (call-process-region (car region) (cdr region)
                                             "pyexecserver"
                                             nil tempbuf nil
                                             "--port" (int-to-string pyexecserver-port)
                                             "--exec-stdin")))
                    0)
                (message "pyexec error: %s" (buffer-substring-no-properties (point-min)
                                                                            (point-max)))
              (message "Python form `%S' executed successfully" form)))))))

(defun pyexec-buffer ()
  (interactive)
  (pyexecserver-send 'buffer))

(defun pyexec-region ()
  (interactive)
  (pyexecserver-send 'region))

(defun pyexec-lines ()
  (interactive)
  (pyexecserver-send 'lines))

(defun pyexec-block ()
  (interactive)
  (pyexecserver-send "block"))

(defun pyexec-block-or-clause ()
  (interactive)
  (pyexecserver-send "block-or-clause"))

(defun pyexec-class ()
  (interactive)
  (pyexecserver-send "class"))

(defun pyexec-clause ()
  (interactive)
  (pyexecserver-send "clause"))

(defun pyexec-def ()
  (interactive)
  (pyexecserver-send "def"))

(defun pyexec-def-or-class ()
  (interactive)
  (pyexecserver-send "def-or-class"))

(defun pyexec-elif-block ()
  (interactive)
  (pyexecserver-send "elif-block"))

(defun pyexec-else-block ()
  (interactive)
  (pyexecserver-send "else-block"))

(defun pyexec-except-block ()
  (interactive)
  (pyexecserver-send "except-block"))

(defun pyexec-expression ()
  (interactive)
  (pyexecserver-send "expression"))

(defun pyexec-for-block ()
  (interactive)
  (pyexecserver-send "for-block"))

(defun pyexec-if-block ()
  (interactive)
  (pyexecserver-send "if-block"))

(defun pyexec-line ()
  (interactive)
  (pyexecserver-send "line"))

(defun pyexec-minor-block ()
  (interactive)
  (pyexecserver-send "minor-block"))

(defun pyexec-paragraph ()
  (interactive)
  (pyexecserver-send "paragraph"))

(defun pyexec-partial-expression ()
  (interactive)
  (pyexecserver-send "partial-expression"))

(defun pyexec-section ()
  (interactive)
  (pyexecserver-send "section"))

(defun pyexec-statement ()
  (interactive)
  (pyexecserver-send "statement"))

(defun pyexec-top-level ()
  (interactive)
  (pyexecserver-send "top-level"))

(defun pyexec-try-block ()
  (interactive)
  (pyexecserver-send "try-block"))


;; (setq py-install-directory
;;       "/home/jarasz/dev/programming/lisp/emacs/emacs-config/python-mode/")

;;; use IPython
;; (setq-default py-shell-name "ipython")
;; (setq-default py-which-bufname "IPython")
; use the wx backend, for both mayavi and matplotlib
;; (setq py-python-command-args
;;   '("--gui=wx" "--pylab=wx" "-colors" "Linux"))
;; (setq py-force-py-shell-name-p nil)

; switch to the interpreter after executing code
;; (setq py-shell-switch-buffers-on-execute-p t)
(setq py-switch-buffers-on-execute-p nil)
; don't split windows
(setq py-split-windows-on-execute-p nil)
; try to automagically figure out indentation
(setq py-smart-indentation t)


(add-hook 'python-mode-hook 'setup-py-mode)

(add-hook 'py-shell-hook 'py-kill-buffer-on-shell-exit)
(add-hook 'py-shell-hook 'comint-mode-keymap-modify)
(add-hook 'py-shell-hook 'py-shell-keymap-modify)
