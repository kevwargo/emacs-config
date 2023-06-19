(defun scratch-log (string &rest objects)
  (let ((msg (apply 'format string objects)))
    (with-current-buffer "*scratch*"
      (goto-char (point-max))
      (insert msg 10)
      (goto-char (point-max))
      (mapc (lambda (win)
              (if (eq (window-buffer win) (current-buffer))
                  (with-selected-window win
                    (goto-char (point-max)))))
            (window-list)))
    msg))

(defmacro scratch-log-expr (expr &optional prefix-fmt &rest prefix-args)
  `(let ((val ,expr)
         (prefix (if ,prefix-fmt
                     (format ,prefix-fmt ,@prefix-args)
                   "")))
     (scratch-log "%s%s: %S" prefix ',expr val)
     val))

(defmacro message-expr (expr &optional prefix-fmt &rest prefix-args)
  `(let ((val ,expr)
         (prefix (if ,prefix-fmt
                     (format ,prefix-fmt ,@prefix-args)
                   "")))
     (message "%s%s: %S" prefix ',expr val)
     val))
