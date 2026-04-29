;; -*- lexical-binding: t -*-

(defvar log-buffer-name "*main-log*")

(define-derived-mode log-mode special-mode "Log")

(defun logfmt (fmt &rest objects)
  (let ((msg (apply 'format fmt objects)))
    (with-current-buffer (get-buffer-create log-buffer-name)
      (unless (derived-mode-p 'log-mode)
        (log-mode))
      (goto-char (point-max))
      (let ((inhibit-read-only t))
        (insert msg 10))
      (mapc (lambda (f)
              (mapc (lambda (w)
                      (if (eq (window-buffer w) (current-buffer))
                          (set-window-point w (point-max))))
                    (window-list f)))
            (frame-list)))
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
