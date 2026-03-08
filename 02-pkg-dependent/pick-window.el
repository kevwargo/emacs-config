;; -*- lexical-binding: t -*-

(require 'term)
(require 'dash)

;; TODO:
;;   - in addition to splitting, allow to expand the picked window
;;   - implement as a minor mode


(defvar pick-window-keys
  '(("q" . "Q") ("a" . "A") ("z" . "Z") ("x" . "X") ("s" . "S") ("w" . "W")
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
  (let ((old-mode-line (default-value 'mode-line-format))
        (map (make-sparse-keymap))
        (bindings (-zip-pair pick-window-keys (window-list)))
        chosen-window split-p handler)
    (setq handler
          (lambda ()
            (interactive)
            (let ((command-key (key-description (this-command-keys-vector))))
              (--each bindings (-let [((select-key . split-key) . window) it]
                                 (if (cond ((string= command-key select-key)
                                            (setq chosen-window window))
                                           ((and allow-split (string= command-key split-key))
                                            (setq chosen-window window
                                                  split-p t)))
                                     (exit-minibuffer)))))))
    (set-keymap-parent map minibuffer-local-map)
    (--each bindings (-let [((select-key . split-key) . window) it]
                       (keymap-set map select-key handler)
                       (if allow-split
                           (keymap-set map split-key handler))
                       (set-window-parameter window 'pick-window-key-select select-key)
                       (set-window-parameter window 'pick-window-key-split (and allow-split split-key))))
    (define-key map [remap self-insert-command] 'pick-window--invalid-key)
    (set-default 'mode-line-format
                 `(,(car old-mode-line)
                   (:eval (pick-window--mode-line))
                   ,@(cdr old-mode-line)))
    (unwind-protect
        (progn
          (read-from-minibuffer (or prompt "Pick window: ") nil map nil t)
          (if split-p
              (setq chosen-window
                    (if (window-left-child (window-parent chosen-window))
                        (split-window-below nil chosen-window)
                      (split-window-right nil chosen-window))))
          (set-window-parameter chosen-window 'pick-window-created-p split-p)
          chosen-window)
      (set-default 'mode-line-format old-mode-line))))

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
  '(help-mode occur-mode inferior-emacs-lisp-mode magit-mode))

(defun pick-window--match (buf &optional action &rest args)
  (let* ((buf (get-buffer buf))
         (alist (cdr-safe action))
         (action (car-safe action))
         (target-mode (buffer-local-value 'major-mode buf))
         (matches-p (and (cdr (window-list))
                         (not (or (string= (buffer-name buf) "*Completions*")
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
                                  (and (provided-mode-derived-p target-mode
                                                                pick-window--reuse-window-modes)
                                       (memq buf (mapcar 'window-buffer (window-list)))))))))
    (pick-window--log "[%s] %s"
                      (if matches-p "MATCH" "NO MATCH")
                      (format-fontify (pick-window--format-buffer buf)
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

(defun pick-window--format-window (&optional window)
  (setq window (window-normalize-window window))
  (let* ((win-name (prin1-to-string window))
         (buf (window-buffer window))
         (buf-name (and buf
                        (string-fontify (buffer-name buf)
                                        'font-lock-comment-face))))
    (save-match-data
      (and (string-match "^#<window \\([0-9]+\\)" win-name)
           (format-fontify
            (font-lock-keyword-face "#%s" (match-string 1 win-name))
            ":"
            buf-name)))))

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

(defun pick-window--mode-line ()
  (let ((select-key (window-parameter nil 'pick-window-key-select))
        (split-key (window-parameter nil 'pick-window-key-split)))
    (if select-key
        (concat
         (string-fontify "select:" 'font-lock-comment-face)
         (string-fontify select-key 'help-key-binding)
         (if split-key
             (concat
              " "
              (string-fontify "split:" 'font-lock-comment-face)
              (string-fontify split-key 'help-key-binding)))))))

(defun pick-window--invalid-key ()
  (interactive)
  (user-error "no window under key %S"
              (key-description (this-single-command-keys))))

(defun pick-window--log (fmt &rest args)
  (let ((log-buffer-name "*pick-window-log*"))
    (apply 'logfmt
           (concat "[%s] " fmt)
           (string-fontify (format-time-string "%Y-%m-%d %H:%M:%S") 'font-lock-doc-face)
           args)))

(setq display-buffer-alist '((pick-window--match . (display-buffer-pick-window))))

(keymap-global-set "C-c c" 'copy-to-window)
(keymap-global-set "C-c x" 'cut-to-window)
(keymap-global-set "C-c b" 'move-buffer-to-window)
(keymap-global-set "C-c s" 'swap-buffers-window)
(keymap-global-set "C-c f" 'find-file-in-window)
(keymap-global-set "C-c D" 'insert-cwd-to-window)
