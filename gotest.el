(defvar-local go-test-verbose nil)

(defun go-toggle-test-verbose ()
  (interactive)
  (let ((new-val (null go-test-verbose)))
    (setq go-test-verbose new-val)
    (message "go-test-verbose set to %S" new-val)))

(defmacro go-test--toggle-arg (arg &optional matcher)
  `(let* ((all (and go-test-args (s-split " " go-test-args)))
          (others (--remove ,(or matcher `(equal it ,arg)) all)))
     (setq all
           (if (equal others all)
               (cons ,arg all)
             others))
     (message "go-test-args set locally to %S"
              (setq-local go-test-args
                          (and all (s-join " " all))))))

(defun go-toggle-test-race ()
  (interactive)
  (go-test--toggle-arg "-race" (member it '("-race" "--race"))))

(defun go-toggle-test-cache ()
  (interactive)
  (go-test--toggle-arg "-count=1" (s-starts-with? "-count" it)))

(defun go-test-enable-line-wrapping ()
  (setq-local truncate-lines nil))

(keymap-set go-mode-map "C-x V" 'go-toggle-test-verbose)
(keymap-set go-mode-map "C-x R" 'go-toggle-test-race)
(keymap-set go-mode-map "C-x C" 'go-toggle-test-cache)

(add-hook 'go-test-mode-hook 'go-test-enable-line-wrapping)

