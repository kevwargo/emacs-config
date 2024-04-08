;;; -*- lexical-binding: t -*-

(require 'dash)
(require 'grep)
(require 's)
(require 'transient)

(defvar findgrep--command "findgrep")

(setq transient-read-with-initial-input t)

;; Main entry and layout

(transient-define-prefix findgrep ()
  ["Options"
   :class transient-columns
   :setup-children findgrep--setup-children]
  ["Main"
   (":" "Regexp" "Regexp" :class findgrep--parameter-regexp)
   ("/" "Directory" "Directory" :class findgrep--parameter-directory)]
  ["Actions"
   ([RET] "Run" findgrep--run)
   ("q" "Quit findgrep" transient-quit-all)])

(defun findgrep--setup-children (children)
  (ignore children)
  (transient-parse-suffixes 'findgrep (read--command-output findgrep--command "--print-elisp-transient")))

(defun read--command-output (cmd &rest args)
  (with-temp-buffer
    (let ((exit-code (apply 'call-process cmd nil t nil args)))
      (if (zerop exit-code)
          (read (buffer-string))
        (error "%S %S exited with code %d: %s"
               cmd args exit-code (buffer-string))))))

;; Suffix classes

;;; Arguments

(defclass findgrep--argument (transient-argument)
  ((mutex-group :initarg :mutex-group :initform nil)))

(cl-defmethod findgrep--argument-variable ((arg findgrep--argument))
  (intern (format "findgrep--:%s" (oref arg argument))))

(cl-defmethod transient-init-value :after ((arg findgrep--argument))
  (let ((var (findgrep--argument-variable arg)))
    (when (local-variable-p var)
      (oset arg value (eval var)))))

(cl-defmethod transient-infix-set :after ((arg findgrep--argument) value)
  (let ((var (findgrep--argument-variable arg)))
    (set (make-local-variable var) value))
  (when-let ((value value)
             (mutex-group (oref arg mutex-group)))
    (--each transient--suffixes
      (if (and (object-of-class-p it 'findgrep--argument)
               (eq (oref it mutex-group) mutex-group)
               (not (eq it arg)))
          (transient-infix-set it nil)))))

(defclass findgrep-switch (transient-switch findgrep--argument)
  ())

(defclass findgrep-option (transient-option findgrep--argument)
  ())

;;; Parameters

(defclass findgrep--parameter (transient-option)
  ())

(cl-defmethod transient-format-value ((param findgrep--parameter))
  (if-let ((value (oref param value)))
      (propertize value 'face 'transient-argument)
    ""))

;;;; Regexp

(defvar-local findgrep--regexp nil)
(defvar-local findgrep--regexp-history nil)

(defclass findgrep--parameter-regexp (findgrep--parameter)
  ())

(cl-defmethod transient-init-value ((param findgrep--parameter-regexp))
  (oset param value
        (setq findgrep--regexp
              (if (use-region-p)
                  (regexp-quote
                   (buffer-substring-no-properties (region-beginning)
                                                   (region-end)))
                findgrep--regexp))))

(cl-defmethod transient-infix-set :after ((param findgrep--parameter-regexp) value)
  (ignore param)
  (setq findgrep--regexp value))

(cl-defmethod transient-infix-read ((param findgrep--parameter-regexp))
  (ignore param)
  (read-string "Regexp: " findgrep--regexp findgrep--regexp-history))

;;;; Directory

(defvar-local findgrep--directory nil)

(defclass findgrep--parameter-directory (findgrep--parameter)
  ())

(cl-defmethod transient-init-value ((param findgrep--parameter-directory))
  (oset param value
        (setq findgrep--directory
              (or findgrep--directory default-directory))))

(cl-defmethod transient-infix-set :after ((param findgrep--parameter-directory) value)
  (ignore param)
  (setq findgrep--directory value))

(cl-defmethod transient-infix-read ((param findgrep--parameter-directory))
  (ido-read-directory-name "Directory: " (oref param value)))

;; Helper functions

(defun findgrep--quote (arg)
  (if (string-match-p "^\\([[:alnum:]_.,:+=^%@-]+\\|!\\)$" arg)
      arg
    (format "'%s'" (string-replace "'" "'\\''" arg))))

;; Execute

(defun findgrep--run ()
  (interactive)
  (let ((default-directory findgrep--directory)
        (regexp (or findgrep--regexp
                    (user-error "No regexp")))
        (args (--keep (and (object-of-class-p it 'findgrep--argument)
                           (transient-infix-value it))
                      transient-current-suffixes)))
    (compilation-start (s-join " " `(,findgrep--command ,@args ,(findgrep--quote regexp)))
                       'grep-mode
                       (let ((orig (buffer-name)))
                         (lambda (compilation-mode-name)
                           (format "*%s-<%s>*" compilation-mode-name orig))))))
