;; -*- lexical-binding: t -*-

(defvar log-buffer-name "*main-log*")

(defvar log-time-p t)
(defvar log-time-default-fmt "%Y-%m-%d %H:%M:%S.%3N")

(define-derived-mode log-mode special-mode "Log")

(defun logfmt (fmt &rest objects)
  (when log-time-p
    (setq fmt (concat "[%s] " fmt))
    (setq objects
          (cons (format-time-string (if (stringp log-time-p)
                                        log-time-p
                                      log-time-default-fmt))
                objects)))
  (let ((msg (apply 'format fmt objects)))
    (with-current-buffer (get-buffer-create log-buffer-name)
      (unless (derived-mode-p 'log-mode)
        (log-mode))
      (goto-char (point-max))
      (let ((inhibit-read-only t))
        (insert msg 10))
      (tail-displayed-buffer))
    msg))

(defmacro log-expr (expr &optional prefix-fmt &rest prefix-args)
  `(let ((val ,expr))
     (logfmt "%s%S: %S"
             ,(if prefix-fmt `(format ,prefix-fmt ,@prefix-args) "")
             ',expr val)
     val))

(defmacro log-args (&rest args)
  `(logfmt ,(mapconcat (lambda (a) (ignore a) "%s: %S") args "\n")
           ,@(mapcan (lambda (a) (list `',a a)) args)))

(defmacro log-args-line (&rest args)
  `(logfmt ,(mapconcat (lambda (a) (ignore a) "%s:%S") args " ")
           ,@(mapcan (lambda (a) (list `',a a)) args)))

(defmacro message-expr (expr &optional prefix-fmt &rest prefix-args)
  `(let ((val ,expr)
         (prefix (format ,(or prefix-fmt "") ,@prefix-args)))
     (message "%s%s: %S" prefix ',expr val)
     val))

(defmacro log-cond (&optional prefix &rest clauses)
  (unless (stringp prefix)
    (setq clauses (cons prefix clauses)
          prefix ""))
  (let ((val (make-symbol "val")))
    `(cond ,@(mapcar
              (lambda (clause)
                `((let ((,val ,(car clause)))
                    (when ,val
                      (logfmt "%s%S: %S" ,prefix ',(car clause) ,val)
                      ,val))
                  ,@(cdr clause)))
              clauses))))

(defun show-log ()
  (interactive)
  (pop-to-buffer (get-buffer-create log-buffer-name)))

(defun log-clear-buffer ()
  (interactive)
  (when-let* ((buf (get-buffer log-buffer-name))
              (inhibit-read-only t))
    (with-current-buffer buf
      (erase-buffer))))

(defun tail-displayed-buffer (&optional buf)
  (with-current-buffer (or buf (current-buffer))
    (goto-char (point-max))
    (mapc #'tail-window (get-buffer-window-list nil nil t))))

(defun tail-window (w)
  ;; TODO: make this persistent after read-from-minibuffer restores window config
  (set-window-start w
                    (save-excursion
                      (goto-char (point-max))
                      (vertical-motion (- scroll-margin (window-body-height w)) w)
                      (point)))
  (set-window-point w (point-max)))

(defun tail-all-logs ()
  (interactive)
  (dolist (w (window-list))
    (if (provided-mode-derived-p
         (buffer-local-value 'major-mode
                             (window-buffer w))
         'log-mode)
        (tail-window w))))
