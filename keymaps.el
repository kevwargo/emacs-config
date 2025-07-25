(require 'cl-macs)

(load "config/common-keymap")
(load "config/vt-keymap")
(load "config/x-keymap")

(defun keymap-prettify (kmap)
  (if (keymapp kmap)
      (list 'keymap
            (mapcar (lambda (e)
                      (if (consp e)
                          (cl-destructuring-bind (key . def) e
                            (cons (key-description (if (sequencep key)
                                                       key
                                                     (vector key)))
                                  (cond
                                   ((keymapp def) (keymap-prettify def))
                                   ((symbolp def) def)
                                   (t 'garbage))))
                        e))
                    (cdr-safe kmap)))))

(defun sh-mode-keymap-modify ()
  (keymap-local-set "C-j" 'newline)
  (keymap-local-set "RET" 'newline)
  (keymap-local-unset "C-c C-x"))

(defun elisp-mode-keymap-modify ()
  (keymap-local-unset "C-M-q")
  (keymap-local-set "C-c RET" 'eval-print-last-sexp)
  (keymap-local-set "C-(" (lambda () (interactive) (insert ?\())))

(defun lisp-mode-keymap-modify ()
  (keymap-local-set "C-(" (lambda () (interactive) (insert ?\())))

(defun conf-mode-keymap-modify ()
  (keymap-local-unset "C-c C-x"))

(defun html-mode-keymap-modify ()
  (dolist (key '("1" "2" "3" "4"))
    (keymap-local-unset (concat "C-c " key))))

(add-hook 'sh-mode-hook 'sh-mode-keymap-modify)
(add-hook 'emacs-lisp-mode-hook 'elisp-mode-keymap-modify)
(add-hook 'lisp-mode-hook 'lisp-mode-keymap-modify)
(add-hook 'conf-mode-hook 'conf-mode-keymap-modify)
(add-hook 'html-mode-hook 'html-mode-keymap-modify)
