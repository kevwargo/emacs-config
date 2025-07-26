(require 'connection)

(defvar *jlp-port* 39012)

(defun jlp-send-string (string)
  (let ((conn (connection-open "localhost" *jlp-port*)))
    (unwind-protect
        (progn
          (connection-send conn string))
      (connection-close conn))))

(defun jlp-send-defun ()
  (interactive)
  (when-let ((d (thing-at-point 'defun t)))
    (jlp-send-string d)))

(defun jlp-send-sexp ()
  (interactive)
  (when-let ((s (thing-at-point 'sexp t)))
    (jlp-send-string s)))

(defun jlp-send-buffer ()
  (interactive)
  (jlp-send-string (buffer-substring-no-properties (point-min) (point-max))))

(define-derived-mode jlp-mode comint-mode "JLP"
  (unless (comint-check-proc (current-buffer))
    (setq-local comint-prompt-regexp "^JLP> ")
    (setq-local comint-prompt-read-only t)
    (make-network-process :name "JLP"
                          :buffer (current-buffer)
                          :host "localhost"
                          :service *jlp-port*
                          :coding 'utf-8)
    (goto-char (point-max))))

(defun jlp ()
  (interactive)
  (let ((buf-name "*jlp*"))
    (with-current-buffer (get-buffer-create buf-name)
      (jlp-mode))
    (pop-to-buffer-same-window buf-name)))
