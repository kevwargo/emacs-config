;; -*- lexical-binding: t -*-

(require 'cl-macs)
(require 'dash)
(require 'magit)
(require 's)

(defvar magit-log--merge-parent-keys '("[" "]"))
(defvar-local magit-log--merge-point nil)

(defun magit-log-goto-merge-parent ()
  (interactive)
  (when-let* ((parent-idx (-elem-index (this-command-keys) magit-log--merge-parent-keys))
              (current-section (magit-current-section))
              (current (oref current-section value))
              (parents (magit--get-parents current)))
    (when (= (length parents) 2)
      (cl-flet ((make-goto-sibling-fn (n)
                  (lambda ()
                    (interactive)
                    (if magit-log--merge-point
                        (magit-log-move-to-revision (format "%s^%s"
                                                            magit-log--merge-point n))))))
        (setq magit-log--merge-point current)
        (magit-log-move-to-revision (nth parent-idx parents))
        (let ((m (make-sparse-keymap)))
          (keymap-set m "<" (make-goto-sibling-fn 1))
          (keymap-set m ">" (make-goto-sibling-fn 2))
          (set-transient-map m t
                             (lambda () (setq magit-log--merge-point nil))
                             "Use %k to navigate between merge siblings"))))))

(defvar-local magit-log--children-traverse-state nil)
(defvar magit-log--children-traverse-key "|")

(defun magit-log-goto-child ()
  (interactive)
  (when-let* ((current (magit-current-section))
              (current-hash (oref current value))
              (child (magit-log--find-child-from current current-hash)))
    (magit-section-goto child)
    (when-let ((next-child (magit-log--find-child-from (magit-current-section) current-hash)))
      (let ((m (make-sparse-keymap))
            (transient-message (format "Press %%k to move to the %s's next child" current-hash)))
        (keymap-set m magit-log--children-traverse-key 'magit-log--traverse-children)
        (setq magit-log--children-traverse-state
              (list current-hash
                    next-child
                    (set-transient-map m t
                                       (lambda ()
                                         (setq magit-log--children-traverse-state nil))
                                       transient-message)))))))

(defun magit-log--traverse-children ()
  (interactive)
  (-let [(parent-hash child exit-transient) magit-log--children-traverse-state]
    (magit-section-goto child)
    (if-let ((next-child (magit-log--find-child-from (magit-current-section)
                                                     parent-hash)))
        (setf (nth 1 magit-log--children-traverse-state)
              next-child)
      (funcall exit-transient))))

(defun magit-log--find-child-from (section parent)
  (--first (member parent (magit--get-parents (oref it value)))
           (magit-section-siblings section 'prev)))

(defun magit--get-parents (commit)
  (s-split " " (magit-git-string "show" "-s" "--format=%p" commit)))

(dolist (k magit-log--merge-parent-keys)
  (keymap-set magit-log-mode-map k 'magit-log-goto-merge-parent))

(keymap-set magit-log-mode-map magit-log--children-traverse-key 'magit-log-goto-child)
(keymap-set magit-log-mode-map "\\" 'magit-log-goto-child)
