(require 'jdee)
(require 'auto-complete)

(defun ac-jdee--build-method-snippet (method)
  (let* ((method (butlast method))
         (name (car method))
         (return (cadr method))
         (args (mapcar (lambda (class)
                         (string-match "\\.?[[:space:]]*\\([a-zA-Z_][][0-9a-zA-Z_]*\\)\\'" class)
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

(defun ac-jdee-candidates ()
  (condition-case err
      (let* ((pair (let ((p (jdee-parse-java-variable-at-point)))
                     (destructuring-bind (c . f) p
                       (if (= (aref c (1- (length c))) ?.)
                           (cons (substring c 0 (1- (length c)))
                                 (cdr p))
                         p))))
             (type (flet ((y-or-n-p (format) nil)) ; pretty dirty hack
                     (jdee-parse-eval-type-of (car pair))))
             (classinfo (car (jdee-complete-invoke-get-class-info type jdee-complete-public)))
             (fields (nth jdee-complete-fields classinfo))
             (methods (mapcar 'ac-jdee--build-method-snippet
                              (nth jdee-complete-methods classinfo))))
        methods)
   (error
     (message "Error during jdee auto-complete: %S" err))))

(defun ac-jdee-action ()
  (let* ((last-complete-string (cdr ac-last-completion))
         (snippet (get-text-property 0 'value last-complete-string)))
    (when snippet
      (delete-char (- 0 (length last-complete-string)))
      (yas-expand-snippet snippet))))

(ac-define-source jdee
  '((candidates . ac-jdee-candidates)
    (prefix . "\\.[[:space:]]*\\([a-zA-Z_]?[0-9a-zA-Z_]*\\)")
    (requires . 0)
    (action . ac-jdee-action)))

(defun ac-jdee-hook ()
  (add-to-list 'ac-sources 'ac-source-jdee)
  (local-set-key (kbd "<C-tab>") 'ac-complete-jdee))

(defun ac-jdee-toggle ()
  (interactive)
  (if (member 'ac-source-jdee ac-sources)
      (setq ac-sources (remove 'ac-source-jdee ac-sources))
    (add-to-list 'ac-sources 'ac-source-jdee)))

(add-hook 'jdee-mode-hook 'ac-jdee-hook)

