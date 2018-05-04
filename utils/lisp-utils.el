(require 'cl)

(defun enclosure-list (list &optional tail)
  (or (and (numberp tail) (>= tail 1))
      (setq tail 1))
  (if (<= (length list) tail)
      list
    `(,(car list) ,(enclosure-list (cdr list) tail))))

(defmacro c*r (what lst)
  (when (or (stringp what)
            (and (symbolp what)
                 (setq what (symbol-name what))))
    (enclosure-list (append (remove-if 'null
                                       (mapcar (lambda (c)
                                                 (cond ((= c ?a) 'car)
                                                       ((= c ?d) 'cdr)))
                                               what))
                            (list lst))
                    2)))

(defun list-startswith-p (sublist list)
  "Return t if first (length `sublist') elements in `list' are equal to sublist
or if `sublist' is nil, nil otherwise."
  (when (and (listp sublist)
             (listp list))
    (if (null sublist)
        t
      (and (equal (car list)
                  (car sublist))
           (list-startswith-p (cdr sublist) (cdr list))))))

(defun find-head (list llist)
  "Find and return the first such element in `llist' `llist' starts with,
nil otherwise."
  (find list llist :test 'list-startswith-p))

;; thanks to “Pascal J Bourguignon” and “TheFlyingDutchman <zzbba...@aol.com>”. 2010-09-02
(defun read-file (file-path)
  "Return FILEPATH's file content."
  (with-temp-buffer
    (insert-file-contents-literally file-path)
    (buffer-substring-no-properties (point-min) (point-max))))

(defun find-head-in-file (list file-path)
  (find-head list (read (concat "(" (read-file file-path) ")"))))

(defun insert-to-list (list elt index)
  "Destructively inserts element `elt' to the list
`list' at the position `index'.
Counting starts from 0. Returns resulting list"
  (when (and (numberp index) (>= index 0))
    (if (= index 0)
        (let ((car-bak (car list)))
          (setcar list elt)
          (setcdr list (cons car-bak (cdr list))))
      (insert-to-list (cdr list) elt (1- index)))
    list))

(defun delete-from-list (list index)
  "Delete element indexed by `index' from `list' destructively."
  (when (and (numberp index) (>= index 0))
    (cond ((= index 0)
           (when (consp list)
             (setcar list (cadr list))
             (setcdr list (cddr list))))
          (t (delete-from-list (cdr list) (1- index))))
    list))

(defun chars-to-strings-in-keymap (keymap)
  (mapcar (lambda (elt)
            (if (not (consp elt))
                elt
              (cons
               (if (characterp (car elt))
                   (char-to-string (car elt))
                 (car elt))
               (if (keymapp (cdr elt))
                   (chars-to-strings-in-keymap (cdr elt))
                 (cdr elt)))))
          keymap))

(defun scratch-log (string &rest objects)
  (with-current-buffer "*scratch*"
    (goto-char (point-max))
    (insert (apply 'format string objects) 10)))

(defun scratch-log-prepend (string &rest objects)
  (with-current-buffer "*scratch*"
    (goto-char (point-min))
    (insert (apply 'format string objects) 10)))

(defun read-whole-string (string)
  (let (result start)
    (condition-case e
        (while t
          (destructuring-bind (sexp . end)
              (read-from-string string start)
            (setq result (append result (list sexp)))
            (setq start end)))
      (error result))))

(provide 'kec:lisp-utils)
