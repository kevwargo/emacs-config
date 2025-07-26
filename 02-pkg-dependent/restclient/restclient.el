(require 'restclient)

(defvar restclient--aws-request-signer-executable
  (expand-file-name "sign_request.py"
                    (file-name-directory load-file-name)))

(defvar-local restclient--aws-request-signer-process nil)
(defvar-local restclient--aws-request-signer-resp nil)
(defvar-local restclient--aws-request-signer-error nil)

(defun restclient--aws-proc-filter (proc string)
  (if (null (string-match-p "^[[:space:]\n]+$" string))
      (let (resp raw-resp)
        (condition-case err
            (setq resp
                  (mapcar (lambda (item) (cons (symbol-name (car item)) (cdr item)))
                          (json-parse-string string :object-type 'alist)))
          (error (setq raw-resp
                       (if (member (car err)
                                   '(json-end-of-file json-parse-error json-trailing-content))
                           (format "JSON(%s): '%s'" (car err) string)
                         (format "err %s: '%s'" err string)))))
        (if resp
            (if-let ((resp-error (cdr (assoc "error" resp))))
                (setq restclient--aws-request-signer-error resp-error)
              (setq restclient--aws-request-signer-resp resp))
          (if-let* ((m (string-match "Enter MFA code for .*: " raw-resp))
                    (mfa-prompt (match-string-no-properties 0 raw-resp)))
              (process-send-string proc
                                   (concat (read-string mfa-prompt) "\n"))
            (setq restclient--aws-request-signer-error raw-resp))))))

(defun restclient--aws-request-signer-send (request)
  (setq restclient--aws-request-signer-resp nil)
  (setq restclient--aws-request-signer-error nil)

  (process-send-string restclient--aws-request-signer-process
                       (concat (json-encode request) "\n"))
  (while (null (or restclient--aws-request-signer-resp restclient--aws-request-signer-error))
    (accept-process-output restclient--aws-request-signer-process))

  (if restclient--aws-request-signer-error
      (error "AWS request signer error: %s" restclient--aws-request-signer-error))
  restclient--aws-request-signer-resp)

(defun restclient--aws-send-request (method url headers entity)
  (if (null (process-live-p restclient--aws-request-signer-process))
      (setq restclient--aws-request-signer-process
            (make-process
             :name "restclient--aws-request-signer"
             :command (list restclient--aws-request-signer-executable)
             :connection-type 'pipe
             :filter 'restclient--aws-proc-filter)))

  (let ((vars (restclient-find-vars-before-point)))
    (setq headers
          (restclient--aws-request-signer-send `((profile . ,(cdr (assoc ":aws-profile" vars)))
                                                 (region . ,(cdr (assoc ":aws-region" vars)))
                                                 (service . ,(cdr (assoc ":aws-service" vars)))
                                                 (method . ,method)
                                                 (url . ,url)
                                                 (headers . ,headers)
                                                 (body . ,entity)))))
  (dolist (h '(("Connection" . "close") ("Content-Type" . "application/json")))
    (if (null (assoc (car h) headers))
        (setq headers (cons h headers))))
  (setq headers (cl-remove-if (lambda (h)
                                (if (string= (car h) "-default-service")
                                    (message "Warning: used the default %s AWS service. Server may throw 403" (cdr h))))
                              headers))
  (restclient-http-do method url headers entity nil nil nil))

(defun restclient-aws-send-current ()
  (interactive)
  (let ((restclient-log-request nil))
    (restclient-http-parse-current-and-do 'restclient--aws-send-request)))

(defun restclient-aws-kill-process ()
  (interactive)
  (if (process-live-p restclient--aws-request-signer-process)
      (message "aws request signer process killed: %S"
               (process-id (kill-process restclient--aws-request-signer-process)))
    (message "no aws request signer process")))

(defun restclient-json-format-body ()
  (interactive)
  (save-excursion
    (goto-char (restclient-current-max))
    (if (save-excursion
          (re-search-backward restclient-method-url-regexp (point-min) t))
        (json-pretty-print (save-excursion (backward-sexp) (point))
                           (point)))))

(keymap-set restclient-mode-map "C-c RET" 'restclient-aws-send-current)
(keymap-set restclient-mode-map "C-c C-k" 'restclient-aws-kill-process)
(keymap-set restclient-mode-map "C-c f" 'restclient-json-format-body)

(add-to-list 'auto-mode-alist '("\\.rest$" . restclient-mode))
