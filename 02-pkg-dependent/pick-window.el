;; -*- lexical-binding: t -*-

(require 'term)
(require 'dash)

;; TODO:
;;   - in addition to splitting, allow to expand the picked window

(define-minor-mode pick-window-mode
  "A minor mode for enabling fast window selecting for pop-to-buffer"
  :lighter nil
  :global t
  :keymap (let ((m (make-sparse-keymap)))
            (keymap-set m "C-c c" 'copy-to-window)
            (keymap-set m "C-c x" 'cut-to-window)
            (keymap-set m "C-c b" 'move-buffer-to-window)
            (keymap-set m "C-c s" 'swap-buffers-window)
            (keymap-set m "C-c f" 'find-file-in-window)
            (keymap-set m "C-c D" 'insert-cwd-to-window)
            (keymap-set m "C-c #" 'pick-window-toggle-numbers)
            m)
  (let ((ml (default-value 'mode-line-format))
        (ml-entry '(:eval (pick-window--mode-line)))
        (dba-entry '(pick-window--match . (display-buffer-pick-window))))
    (cond (pick-window-mode
           (unless (member ml-entry ml)
             (set-default 'mode-line-format (append (list (car ml) ml-entry) (cdr ml))))
           (add-to-list 'display-buffer-alist dba-entry))
          (t
           (set-default 'mode-line-format (remove ml-entry ml))
           (setq display-buffer-alist (remove dba-entry display-buffer-alist))))))

(defvar pick-window-keys
  '(("q" . "Q") ("w" . "W") ("a" . "A") ("s" . "S") ("z" . "Z") ("x" . "X")
    ("e" . "E") ("d" . "D") ("c" . "C") ("v" . "V") ("f" . "F") ("r" . "R")
    ("t" . "T") ("g" . "G") ("b" . "B") ("n" . "N") ("h" . "H") ("y" . "Y")
    ("u" . "U") ("j" . "J") ("m" . "M") ("k" . "K") ("i" . "I") ("l" . "L")
    ("o" . "O") ("p" . "P"))
  "An alist of keys, where each element's car is a key that chooses the window,
while the cdr is the key that splits it and chooses the created child window.")

(defun display-buffer-pick-window (buf alist)
  (when (eq this-command 'isearch-occur)
    (pick-window--disable-isearch))
  (when-let* ((w (pick-window t (format "Pick window for %S: " buf))))
    (window--display-buffer buf w
                            (if (window-parameter w 'pick-window-created-p)
                                'window
                              'reuse)
                            alist)))

(defun pick-window (&optional allow-split prompt)
  (let ((map (make-sparse-keymap))
        (pick-window--active-p t)
        chosen-window split-p)
    (set-keymap-parent map minibuffer-local-map)
    (--each (-zip-pair pick-window-keys (window-list))
      (-let [((select-key . split-key) . window) it]
        (keymap-set map select-key
                    (lambda ()
                      (interactive)
                      (setq chosen-window window)
                      (exit-minibuffer)))
        (if allow-split
            (keymap-set map split-key
                        (lambda ()
                          (interactive)
                          (setq chosen-window window
                                split-p t)
                          (exit-minibuffer))))
        (set-window-parameter window 'pick-window-key-select select-key)
        (set-window-parameter window 'pick-window-key-split
                              (if allow-split split-key))))
    (define-key map [remap self-insert-command]
                (lambda ()
                  (interactive)
                  (user-error "no window under key %S"
                              (key-description (this-single-command-keys)))))
    (read-from-minibuffer (or prompt "Pick window: ") nil map nil t)
    (if split-p
        (setq chosen-window
              (if (window-left-child (window-parent chosen-window))
                  (split-window-below nil chosen-window)
                (split-window-right nil chosen-window))))
    (set-window-parameter chosen-window 'pick-window-created-p split-p)
    chosen-window))

(defun pick-window-toggle-numbers ()
  (interactive)
  (setq pick-window--show-numbers (not pick-window--show-numbers))
  (force-mode-line-update t))

(defun insert-to-window (beg end window &optional cut)
  "Inserts selected text to the buffer in the window in direction DIR
and selects that window.

If CUT is non-nil, deletes selected text in current buffer."
  (let ((text (buffer-substring-no-properties beg end)))
    (if cut (delete-region beg end))
    (deactivate-mark)
    (select-window window)
    (if (derived-mode-p 'term-mode)
        (term-send-raw-string text)
      (insert text))))

(defun force-use-region ()
  (or (and (use-region-p)
           (list (region-beginning) (region-end)))
      (error "Region is not active")))

(defun copy-to-window (beg end window)
  (interactive (append (force-use-region) (list (pick-window nil "Copy to window: "))))
  (insert-to-window beg end window))

(defun cut-to-window (beg end window)
  (interactive (append (force-use-region) (list (pick-window nil "Cut to window: "))))
  (insert-to-window beg end window t))

(defun move-buffer-to-window (window)
  (interactive (list (pick-window t (format "Move %S to: " (current-buffer)))))
  (let ((buf (current-buffer))
        (orig-pos (point)))
    (previous-buffer)
    (select-window window)
    (switch-to-buffer buf)
    (goto-char orig-pos)))

(defun swap-buffers-window (window)
  (interactive (list (pick-window nil (format "Swap %S with: " (current-buffer)))))
  (let ((this-buffer (current-buffer)))
    (switch-to-buffer (window-buffer window) nil t)
    (select-window window)
    (switch-to-buffer this-buffer nil t)))

(defun find-file-in-window (window)
  (interactive (list (pick-window t "Find file in: ")))
  (let ((cwd (buffer-working-directory)))
    (select-window window)
    (let ((default-directory cwd))
      (ido-find-file))))

(defun insert-cwd-to-window (window)
  (interactive (list (pick-window t "Insert cwd to: ")))
  (let ((wd default-directory))
    (select-window window)
    (if (derived-mode-p 'term-mode)
        (term-send-raw-string wd)
      (insert wd))))


(defvar pick-window--disabled-modes
  '(debugger-mode xref--xref-buffer-mode))

(defvar pick-window--reuse-window-modes
  '(help-mode occur-mode comint-mode magit-mode))

(defun pick-window--reuse-p (target-buf)
  (and (memq target-buf (mapcar 'window-buffer (window-list)))
       (or (provided-mode-derived-p (buffer-local-value 'major-mode target-buf)
                                    pick-window--reuse-window-modes)
           (memq this-command '(occur-mode-goto-occurrence
                                compile-goto-error
                                recompile
                                undo-tree-visualize-undo
                                undo-tree-visualize-redo)))))

(defun pick-window--match (buf &optional action &rest args)
  (let* ((buf (get-buffer buf))
         (alist (cdr-safe action))
         (action (car-safe action))
         (target-mode (buffer-local-value 'major-mode buf))
         (matches-p (and (cdr (window-list))
                         (not (or (member (buffer-name buf) '("*Warnings*" "*Completions*"))
                                  (--any? (cdr (assq it alist)) '(side dedicated))
                                  (provided-mode-derived-p target-mode pick-window--disabled-modes)
                                  (and (provided-mode-derived-p target-mode 'magit-diff-mode)
                                       (buffer-file-name)
                                       (string-match-p git-commit-filename-regexp
                                                       (buffer-file-name))
                                       (string= (magit-toplevel)
                                                (with-current-buffer buf (magit-toplevel))))
                                  (and (provided-mode-derived-p target-mode 'process-menu-mode)
                                       (not (eq this-command 'list-processes)))
                                  (pick-window--reuse-p buf))))))
    (pick-window--log "%s" (format-fontify ("[%s] " (if matches-p "MATCH" "NO MATCH"))
                                           (pick-window--format-buffer buf)
                                           " action:"
                                           (font-lock-comment-face "%S" action)
                                           " alist:"
                                           (font-lock-comment-face "%S" alist)
                                           (" visible-buffers: (%s)"
                                            (mapconcat 'pick-window--format-window (window-list) " "))
                                           " this-command:"
                                           (font-lock-string-face "%S" this-command)
                                           " current-buffer:"
                                           (pick-window--format-buffer)))
    matches-p))

(defun pick-window--win-num (window)
  (let ((win-name (prin1-to-string window)))
    (save-match-data
      (if (string-match "^#<window \\([0-9]+\\)" win-name)
          (match-string 1 win-name)
        win-name))))

(defun pick-window--format-window (&optional window)
  (setq window (window-normalize-window window))
  (let* ((buf (window-buffer window))
         (buf-name (and buf
                        (string-fontify (concat ":" (buffer-name buf))
                                        'font-lock-comment-face)))
         (win-num (pick-window--win-num window)))
    (format-fontify (font-lock-keyword-face "#%s" win-num) buf-name)))

(defun pick-window--format-buffer (&optional buffer)
  (setq buffer (if buffer (get-buffer buffer) (current-buffer)))
  (format-fontify (font-lock-keyword-face "%S" buffer)
                  (font-lock-variable-name-face
                   "%S" (buffer-local-value 'major-mode buffer))))

(defun pick-window--disable-isearch ()
  (--each (buffer-list)
    (with-current-buffer it
      (when isearch-mode
        (pick-window--log "disabling isearch-mode in %S" it)
        (isearch-done)
        (isearch-clean-overlays)))))

(defvar pick-window--active-p nil)

(defvar pick-window--show-numbers nil
  "Whether to show the number of each window it its corresponding mode-line")

(defun pick-window--mode-line ()
  (concat
   (if pick-window--show-numbers
       (concat " "
               (string-fontify (pick-window--win-num (selected-window))
                               'font-lock-function-name-face)))
   (if pick-window--active-p
       (let ((select-key (window-parameter nil 'pick-window-key-select))
             (split-key (window-parameter nil 'pick-window-key-split)))
         (if select-key
             (concat
              (string-fontify " select:" 'font-lock-comment-face)
              (string-fontify select-key 'help-key-binding)
              (if split-key
                  (concat
                   " "
                   (string-fontify "split:" 'font-lock-comment-face)
                   (string-fontify split-key 'help-key-binding)))))))))

(defun pick-window--log (fmt &rest args)
  (let ((log-buffer-name "*pick-window-log*"))
    (apply 'logfmt
           (concat "[%s] " fmt)
           (string-fontify (format-time-string "%Y-%m-%d %H:%M:%S") 'font-lock-doc-face)
           args)))

(pick-window-mode 1)
