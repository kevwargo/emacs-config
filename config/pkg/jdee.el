(require 'jdee)
(require 'auto-complete)

(defun ac-jdee-candidates ()
  (ignore-errors
    (let* ((pair (jdee-parse-java-variable-at-point))
           (type (flet ((y-or-n-p (format) nil)) ; pretty dirty hack
                   (jdee-parse-eval-type-of (car pair))))
           (classinfo (car (jdee-complete-invoke-get-class-info type jdee-complete-public)))
           (fields (nth jdee-complete-fields classinfo))
           (methods (nth jdee-complete-methods classinfo)))
      (append
       (mapcar (lambda (method)
                 (let* ((method (butlast method))
                        (name (car method))
                        (return (cadr method))
                        (args (mapcar (lambda (class)
                                        (string-match "\\.?\\([a-zA-Z_][0-9a-zA-Z_]*\\)\\'" class)
                                        (match-string 1 class))
                                      (cddr method))))
                   (cons
                    (concat name "(" (mapconcat 'identity args ", ") ")")
                    (concat name "("
                            (mapconcat
                             'identity
                             (let ((index 1)
                                   (arg args)
                                   list)
                               (while (car arg)
                                 (setq list (append list (list (format "${%d:%s}"
                                                                       index
                                                                       (car arg)))))
                                 (setq index (1+ index))
                                 (setq arg (cdr arg)))
                               list)
                             ", ")
                            ")$0"))))
               methods)))))

(defun ac-jdee-action ()
  (let* ((last-complete-string (cdr ac-last-completion))
         (snippet (get-text-property 0 'value last-complete-string)))
    (when snippet
      (delete-char (- 0 (length last-complete-string)))
      (yas-expand-snippet snippet))))

(ac-define-source jdee
  '((candidates . ac-jdee-candidates)
    (prefix . "\\.[\\n\\r\\t ]*\\([a-zA-Z_]?[0-9a-zA-Z_]*\\)")
    (requires . 0)
    (action . ac-jdee-action)))

(defun ac-jdee-hook ()
  (add-to-list 'ac-sources 'ac-source-jdee)
  (local-set-key (kbd "<C-tab>") 'ac-complete-jdee))

(add-hook 'jdee-mode-hook 'ac-jdee-hook)

