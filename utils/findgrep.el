(defvar-local findgrep-dir nil)

(defvar-local findgrep-grep-opts
  '(("-H")
    ("-n")))

(defvar-local findgrep-find-opts
  '(("-type" . "f")
    ("! -name" . "*~")
    ("! -name" . "#*#")))

(defvar-local findgrep-regex "")

(defvar-local findgrep-whole-word nil)

(defvar-local findgrep-names
  (list
   "*.php"
   "*.js"
   "*.ts"
   "*.html"
   "*.java"
   "*.lisp"
   "*.py"
   "*.c"
   "*.cpp"
   "*.go"
   "*.h"
   "*.el"))

(defvar-local findgrep-current-name nil)

(defvar-local findgrep-keys
  '(((kbd "C-r") . fg-reset-opts-cmd)
    ((kbd "C-#") . fg-toggle-ignore-autosave-cmd)
    ((kbd "C-~") . fg-toggle-ignore-backup-cmd)
    ((kbd "<C-up>") . fg-prev-name-cmd)
    ((kbd "<C-down>") . fg-next-name-cmd)
    ((kbd "C-c") . fg-set-name-c-cmd)
    ((kbd "C-S-c") . fg-set-name-cpp-cmd)
    ((kbd "C-S-h") . fg-set-name-h-cmd)
    ((kbd "C-S-g") . fg-set-name-go-cmd)
    ((kbd "C-M-n") . fg-enable-name-cmd)
    ((kbd "C-M-S-n") . fg-disable-name-cmd)
    ((kbd "C-S-n") . fg-toggle-exclude-node-modules-cmd)
    ((kbd "C-S-v") . fg-toggle-exclude-vendor-cmd)
    ((kbd "C-S-i") . fg-toggle-case-insensitive-cmd)
    ((kbd "C-S-p") . fg-toggle-perl-regex-cmd)
    ((kbd "C-S-e") . fg-toggle-extended-regex-cmd)))


(defun findgrep-reset-vars-in-all-buffers ()
  (interactive)
  (dolist (buf (buffer-list))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (setq findgrep-dir (default-value 'findgrep-dir))
        (setq findgrep-grep-opts (default-value 'findgrep-grep-opts))
        (setq findgrep-find-opts (default-value 'findgrep-find-opts))
        (setq findgrep-regex (default-value 'findgrep-regex))
        (setq findgrep-names (default-value 'findgrep-names))
        (setq findgrep-keys (default-value 'findgrep-keys))))))

(defmacro with-minibuffer-origin (&rest body)
  (declare (indent 0))
  `(with-current-buffer (window-buffer (minibuffer-selected-window))
     ,@body))

(defun build-opt-string (opts)
  (let* ((opt-filter (lambda (opt)
                       (and (consp opt)
                            (stringp (car opt))
                            (null (cdr opt))
                            (string-match-p "^-[a-zA-Z]$" (car opt)))))
         (short (mapconcat (lambda (opt)
                             (substring-no-properties (car opt) 1 2))
                           (remove-if-not opt-filter opts)
                           ""))
         (short (if (string= short "") nil short))
         (long (mapconcat (lambda (opt)
                            (if (and (consp opt) (stringp (car opt)))
                                (if (stringp (cdr opt))
                                    (concat (car opt) " " (shell-quote-argument (cdr opt)))
                                  (car opt))))
                          (remove-if opt-filter opts)
                          " "))
         (opt-string ""))
    (if short (setq opt-string (concat "-" short)))
    (if (not (string= long ""))
        (setq opt-string (concat opt-string
                                 (if short " " "")
                                 long)))
    opt-string))

(defun findgrep-build-regex ()
  (if findgrep-whole-word
      (concat "\\\\\\<" findgrep-regex "\\\\\\>")
    findgrep-regex))

(defun findgrep-build-cmdline ()
  (let* ((regex (findgrep-build-regex))
         (cmd (concat
               "cd "
               (or findgrep-dir default-directory)
               " && find . "
               (build-opt-string (append findgrep-find-opts
                                         (if findgrep-current-name
                                             `(("-name" . ,findgrep-current-name)))))
               " -exec grep "
               (build-opt-string findgrep-grep-opts)
               " "))
         (pos (+ (1+ (length cmd))
                 (length regex))))
    (cons (concat cmd regex " {} +") pos)))

(defun findgrep-update-cmdline (&optional regex)
  (when (active-minibuffer-window)
    (let ((cmd (with-minibuffer-origin
                 (let ((findgrep-regex (if regex regex findgrep-regex)))
                   (findgrep-build-cmdline)))))
      (delete-minibuffer-contents)
      (insert (car cmd))
      (goto-char (+ (minibuffer-prompt-end) (1- (cdr cmd))))
      (when regex
        (let ((new-mark (- (point) (length regex))))
          (set-mark new-mark)
          (activate-mark))))))

(defun findgrep-add-opt (opts-var opt value)
  (add-to-list opts-var (cons opt value)))

(defun findgrep--opt-predicate (opt)
  (lambda (item)
    (and (consp item)
         (stringp (car item))
         (string= (car item) opt))))

(defun findgrep--opt-value-predicate (opt value)
  (lambda (item)
    (and (consp item)
         (stringp (car item))
         (string= (car item) opt)
         (equalp (cdr item) value))))

(defun findgrep-opt-set-p (opts-var opt)
  (find-if (findgrep--opt-predicate opt)
           (eval opts-var)))

(defun findgrep-opt-value-set-p (opts-var opt value)
  (find-if (findgrep--opt-value-predicate opt value)
           (eval opts-var)))

(defun findgrep-remove-opt (opts-var opt)
  (set opts-var
       (remove-if (findgrep--opt-predicate opt)
                  (eval opts-var))))

(defun findgrep-remove-opt-value (opts-var opt value)
  (set opts-var
       (remove-if (findgrep--opt-value-predicate opt value)
                  (eval opts-var))))

(defun findgrep-toggle-opt (opts-var opt value)
  (if (findgrep-opt-value-set-p opts-var opt value)
      (findgrep-remove-opt-value opts-var opt value)
    (findgrep-add-opt opts-var opt value)))

(defmacro define-findgrep-cmd (name args &rest body)
  (declare (indent 2))
  `(,@(if name
          `(defun ,(intern (concat "fg-" (symbol-name name) "-cmd")))
        '(lambda))
    ,args
    ,@(if (eq (car (car body)) 'interactive)
          (prog1 (list `(interactive ,@(cdr (car body))))
            (setq body (cdr body)))
        (list '(interactive)))
    (with-minibuffer-origin
      ,@body)
    (findgrep-update-cmdline (if (region-active-p)
                                 (buffer-substring-no-properties
                                  (region-beginning)
                                  (region-end))))))

(define-findgrep-cmd reset-opts ()
  (setq findgrep-grep-opts (default-value 'findgrep-grep-opts))
  (setq findgrep-find-opts (default-value 'findgrep-find-opts)))

(define-findgrep-cmd toggle-ignore-backup ()
  (findgrep-toggle-opt 'findgrep-find-opts "! -name" "*~"))

(define-findgrep-cmd toggle-ignore-autosave ()
  (findgrep-toggle-opt 'findgrep-find-opts "! -name" "#*#"))

(define-findgrep-cmd toggle-exclude-node-modules ()
  (findgrep-toggle-opt 'findgrep-find-opts "! -path" "*/node_modules/*"))

(define-findgrep-cmd toggle-exclude-vendor ()
  (findgrep-toggle-opt 'findgrep-find-opts "! -path" "*/vendor/*"))

(define-findgrep-cmd toggle-case-insensitive ()
  (findgrep-toggle-opt 'findgrep-grep-opts "-i" nil))

(define-findgrep-cmd toggle-extended-regex ()
  (findgrep-toggle-opt 'findgrep-grep-opts "-E" nil))

(define-findgrep-cmd toggle-perl-regex ()
  (findgrep-toggle-opt 'findgrep-grep-opts "-P" nil))

(define-findgrep-cmd enable-name ()
  (setq findgrep-current-name (car findgrep-names)))

(define-findgrep-cmd disable-name ()
  (setq findgrep-current-name nil))

(define-findgrep-cmd next-name ()
  (setq findgrep-names (append (cdr findgrep-names)
                               (list (car findgrep-names))))
  (setq findgrep-current-name (car findgrep-names)))

(define-findgrep-cmd prev-name ()
  (setq findgrep-names (append (last findgrep-names)
                               (butlast findgrep-names)))
  (setq findgrep-current-name (car findgrep-names)))

(define-findgrep-cmd set-name-c ()
  (setq findgrep-current-name "*.c"))
(define-findgrep-cmd set-name-cpp ()
  (setq findgrep-current-name "*.cpp"))
(define-findgrep-cmd set-name-h ()
  (setq findgrep-current-name "*.h"))
(define-findgrep-cmd set-name-go ()
  (setq findgrep-current-name "*.go"))

(defun grep-unset-grep-options ()
  (setenv "GREP_OPTIONS" nil))

(add-hook 'grep-setup-hook 'grep-unset-grep-options)

(defun findgrep (dir regex &optional whole-word)
  (interactive
   (list (ido-read-directory-name "Findgrep dir: " (or findgrep-dir default-directory))
         (if (region-active-p)
             (shell-quote-argument
              (regexp-quote
               (buffer-substring-no-properties
                (region-beginning)
                (region-end))))
           "")
         current-prefix-arg))
  (setq findgrep-dir dir)
  (setq findgrep-whole-word whole-word)
  (setq findgrep-regex regex)
  (let ((null-device nil))
    (grep (read-from-minibuffer "FindGrep: "
                                (findgrep-build-cmdline)
                                (let ((map (make-sparse-keymap)))
                                  (dolist (keydef findgrep-keys)
                                    (destructuring-bind (key . def) keydef
                                      (define-key map
                                        (if (consp key) (eval key) key)
                                        (if (consp def)
                                            (eval `(define-findgrep-cmd nil nil ,def))
                                          def))))
                                  (define-key map [up] 'previous-history-element)
                                  (define-key map [down] 'next-history-element)
                                  (define-key map [10] 'exit-minibuffer)
                                  (define-key map [13] 'exit-minibuffer)
                                  (define-key map [7] 'minibuffer-keyboard-quit)
                                  map)
                                nil
                                'grep-find-history))))
