;;; -*- lexical-binding: t -*-

(require 'cl-generic)
(require 'transient)
(require 'dash)

(defvar-local findgrep-directory nil)
(defvar-local findgrep-regexp nil)

(defvar-local findgrep-exclude-cdk-out t)
(defvar-local findgrep-exclude-cover t)
(defvar-local findgrep-exclude-node-modules t)
(defvar-local findgrep-exclude-serverless t)
(defvar-local findgrep-exclude-autosave t)
(defvar-local findgrep-exclude-temp t)
(defvar-local findgrep-exclude-temp-sockets t)
(defvar-local findgrep-exclude-js nil)
(defvar-local findgrep-exclude-d-ts nil)
(defvar-local findgrep-exclude-lock-files t)
(defvar-local findgrep-exclude-go-test-files nil)
(defvar-local findgrep-include-go nil)
(defvar-local findgrep-include-graphql nil)
(defvar-local findgrep-include-typescript nil)
(defvar-local findgrep-include-javascript nil)
(defvar-local findgrep-include-emacs-lisp nil)
(defvar-local findgrep-include-java nil)
(defvar-local findgrep-include-python nil)

(defvar-local findgrep-ignore-case nil)
(defvar-local findgrep-filenames-only nil)
(defvar-local findgrep-extended-regexp nil)
(defvar-local findgrep-perl-regexp nil)
(defvar-local findgrep-whole-word nil)
(defvar-local findgrep-show-filenames t)
(defvar-local findgrep-show-line-numbers t)
(defvar-local findgrep-ignore-binary-files t)

(defvar-local findgrep--history nil)

(transient-define-prefix findgrep ()
  "Run a complex find-grep command in a directory."
  [["Exclude paths"
    ("c" "Exclude cdk.out directories" "cdk.out"
     :class findgrep--switch-exclude-path
     :value-var findgrep-exclude-cdk-out)
    ("v" "Exclude cover directories" "cover"
     :class findgrep--switch-exclude-path
     :value-var findgrep-exclude-cover)
    ("n" "Exclude node_modules directories" "node_modules"
     :class findgrep--switch-exclude-path
     :value-var findgrep-exclude-node-modules)
    ("s" "Exclude .serverless directories" ".serverless"
     :class findgrep--switch-exclude-path
     :value-var findgrep-exclude-serverless)]
   ["Exclude files"
    ("~" "Exclude autosave files" "*~"
     :class findgrep--switch-exclude-name
     :value-var findgrep-exclude-autosave)
    ("#" "Exclude Emacs temp files" "#*#"
     :class findgrep--switch-exclude-name
     :value-var findgrep-exclude-temp)
    (".#" "Exclude Emacs temp sockets" ".#*#"
     :class findgrep--switch-exclude-name
     :value-var findgrep-exclude-temp-sockets)
    ("j" "Exclude *.js files" "*.js"
     :class findgrep--switch-exclude-name
     :value-var findgrep-exclude-js)
    ("D" "Exclude *.d.ts files" "*.d.ts"
     :class findgrep--switch-exclude-name
     :value-var findgrep-exclude-d-ts)
    ("l" "Exclude package-lock.json files" "package-lock.json"
     :class findgrep--switch-exclude-name
     :value-var findgrep-exclude-lock-files)
    ("T" "Exclude *_test.go files" "*_test.go"
     :class findgrep--switch-exclude-name
     :value-var findgrep-exclude-go-test-files)]
   ["Select files"
    ("g" "Search *.go files only" "*.go"
     :class findgrep--switch-include-name
     :value-var findgrep-include-go)
    ("q" "Search *.graphql files only" "*.graphql"
     :class findgrep--switch-include-name
     :value-var findgrep-include-graphql)
    ("t" "Search *.ts files only" "*.ts"
     :class findgrep--switch-include-name
     :value-var findgrep-include-typescript)
    ("R" "Search *.js files only" "*.js"
     :class findgrep--switch-include-name
     :value-var findgrep-include-javascript)
    ("E" "Search *.el files only" "*.el"
     :class findgrep--switch-include-name
     :value-var findgrep-include-emacs-lisp)
    ("J" "Search *.java files only" "*.java"
     :class findgrep--switch-include-name
     :value-var findgrep-include-java)
    ("p" "Search *.py files only" "*.py"
     :class findgrep--switch-include-name
     :value-var findgrep-include-python)]
   ["Grep args"
    ("-i" "Ignore case" "-i"
     :class findgrep--switch-grep
     :value-var findgrep-ignore-case)
    ("-l" "Filenames only" "-l"
     :class findgrep--switch-grep
     :value-var findgrep-filenames-only)
    ("-E" "Extended regexp" "-E"
     :class findgrep--switch-grep
     :value-var findgrep-extended-regexp)
    ("-P" "Perl regexp" "-P"
     :class findgrep--switch-grep
     :value-var findgrep-perl-regexp)
    ("-w" "Match whole word" "-w"
     :class findgrep--switch-grep
     :value-var findgrep-whole-word)
    ("-H" "Show filename" "-H"
     :class findgrep--switch-grep
     :value-var findgrep-show-filenames)
    ("-n" "Show line numbers" "-n"
     :class findgrep--switch-grep
     :value-var findgrep-show-line-numbers)
    ("-I" "Ignore binary files" "-I"
     :class findgrep--switch-grep
     :value-var findgrep-ignore-binary-files)]]
  ["Main"
   ("d" "Directory" "dir" :class findgrep--infix-dir)
   ("r" "Regexp" "regexp" :class findgrep--infix-regexp)]
  ["Actions"
   ([RET] "Run" findgrep--run)
   ("e" "Edit & Run" findgrep--edit-run)
   ("X" "Replace" findgrep--replace)
   ("/" "Show cmdline" findgrep--show-cmdline)])

(defclass findgrep--switch (transient-switch)
  ((value-var :initarg :value-var))
  :abstract t)
(cl-defmethod transient-init-value ((obj findgrep--switch))
  (oset obj value
        (and (slot-boundp obj 'value-var)
             (boundp (oref obj value-var))
             (if (symbol-value (oref obj value-var))
                 (oref obj argument)))))
(cl-defmethod transient-infix-set :after ((obj findgrep--switch) value)
  (if (slot-boundp obj 'value-var)
      (set (oref obj value-var)
           (oref obj value))))

(defclass findgrep--switch-find (findgrep--switch)
  ()
  :abstract t)

(cl-defgeneric findgrep--list-value ())

(cl-defmethod transient-infix-value ((obj findgrep--switch-find))
  (when (oref obj value)
    (findgrep--list-value obj)))

(defclass findgrep--switch-exclude-path (findgrep--switch-find) ())
(cl-defmethod findgrep--list-value ((obj findgrep--switch-exclude-path))
  (list "!" "-path" (format "*/%s/*" (oref obj argument))))

(defclass findgrep--switch-name (findgrep--switch-find) ())

(defclass findgrep--switch-exclude-name (findgrep--switch-name) ())
(cl-defmethod findgrep--list-value ((obj findgrep--switch-exclude-name))
  (let* ((arg (oref obj argument)))
    (mapcan (lambda (n) `("!" "-name" ,n))
            (if (listp arg) arg (list arg)))))

(defclass findgrep--switch-include-name (findgrep--switch-name) ())
(cl-defmethod findgrep--list-value ((obj findgrep--switch-include-name))
  (list "-name" (oref obj argument)))

(defclass findgrep--switch-grep (findgrep--switch) ())

(defclass findgrep--infix-option (transient-option) ())
(cl-defmethod transient-infix-value ((obj findgrep--infix-option))
  (oref obj value))

(defclass findgrep--infix-regexp (findgrep--infix-option)
  ((always-read :initform t)
   (allow-empty :initform nil)))

(cl-defmethod transient-init-value ((obj findgrep--infix-regexp))
  (oset obj value (if (use-region-p)
                      (regexp-quote (buffer-substring-no-properties
                                     (region-beginning)
                                     (region-end)))
                    findgrep-regexp)))

(cl-defmethod transient-format-value ((obj findgrep--infix-regexp))
  (or (oref obj value) ""))

(cl-defmethod transient-infix-set :after ((obj findgrep--infix-regexp) value)
  (setq findgrep-regexp value))

(defclass findgrep--infix-dir (findgrep--infix-option) ())

(cl-defmethod transient-init-value ((obj findgrep--infix-dir))
  (oset obj value
        (or findgrep-directory
            default-directory)))

(cl-defmethod transient-format-value ((obj findgrep--infix-dir))
  (or (oref obj value) ""))

(cl-defmethod transient-infix-read ((obj findgrep--infix-dir))
  (ido-read-directory-name "Directory: " (oref obj value)))

(cl-defmethod transient-infix-set :after ((obj findgrep--infix-dir) value)
  (setq findgrep-directory value))

(defun findgrep--extract-arg-values (class)
  (delq nil (mapcar (lambda (arg)
                      (if (object-of-class-p arg class)
                          (transient-infix-value arg)))
                    transient-current-suffixes)))

(defun findgrep--smart-quote (arg)
  (if (string-match-p "^\\([[:alnum:]_.,:+=^%@-]+\\|!\\)$" arg)
      arg
    (format "'%s'" (string-replace "'" "'\\''" arg))))

(defun findgrep-regexp ()
  (let ((regexp (findgrep--extract-arg-values 'findgrep--infix-regexp)))
    (if (consp regexp)
        (car regexp)
      (user-error "No regexp provided"))))

(defun findgrep--build-cmdline ()
  (let ((find-path-opts (mapconcat (lambda (arg)
                                     (mapconcat 'findgrep--smart-quote arg " "))
                                   (findgrep--extract-arg-values 'findgrep--switch-exclude-path)
                                   " "))
        (find-name-opts (mapconcat (lambda (arg)
                                     (mapconcat 'findgrep--smart-quote arg " "))
                                   (findgrep--extract-arg-values 'findgrep--switch-name)
                                   " "))
        (grep-opts (apply 'string ?- (mapcar (lambda (arg)
                                               (aref arg 1))
                                             (findgrep--extract-arg-values 'findgrep--switch-grep))))
        (dir (car-safe (findgrep--extract-arg-values 'findgrep--infix-dir))))
    (format "cd %s && find . %s -type f %s -exec grep --color=always %s %s {} +"
            (findgrep--smart-quote dir)
            find-path-opts
            find-name-opts
            grep-opts
            (findgrep--smart-quote (findgrep-regexp)))))

(defun findgrep--show-cmdline ()
  (interactive)
  (message (findgrep--build-cmdline)))

(defun findgrep--run-command (&optional edit)
  (compilation-start (let ((cmd (findgrep--build-cmdline)))
                       (if edit
                           (read-from-minibuffer "Findgrep command: " cmd nil nil 'findgrep--history)
                         cmd))
                     'grep-mode
                     (let ((bufname (buffer-name (current-buffer))))
                       (lambda (mode-name)
                         (format "*%s-<%s>*" mode-name bufname)))))

(defun findgrep--run ()
  (interactive)
  (findgrep--run-command))

(defun findgrep--edit-run ()
  (interactive)
  (findgrep--run-command t))

(defun findgrep--replace (replacement)
  (interactive (list (read-string "Replace with: ")))
  (cl-flet* ((find-files ()
                         (with-temp-buffer
                           (let* ((args (-flatten (list
                                                   (findgrep--extract-arg-values 'findgrep--infix-dir)
                                                   (findgrep--extract-arg-values 'findgrep--switch-exclude-path)
                                                   "-type" "f"
                                                   (findgrep--extract-arg-values 'findgrep--switch-name)
                                                   "-printf" "\"%p\"")))
                                  (code (apply 'call-process
                                               "find" nil t nil
                                               args)))
                             (unless (equal code 0)
                               (user-error "find finished with %s: %s"
                                           code (buffer-substring-no-properties (point-min) (point-max)))))
                           (goto-char (point-min))
                           (insert "(")
                           (goto-char (point-max))
                           (insert ")")
                           (goto-char (point-min))
                           (read (current-buffer))))
             (fetcher ()
                      (let ((files (find-files)))
                        (if files
                            (xref-matches-in-files (findgrep-regexp) files)
                          (user-error "No files found")))))
    (with-current-buffer (xref--show-xrefs #'fetcher nil)
      (xref-query-replace-in-results (findgrep-regexp) replacement))))
