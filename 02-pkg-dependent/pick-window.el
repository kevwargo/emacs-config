;; -*- lexical-binding: t -*-

(require 'term)
(require 'dash)

;; TODO: disable for temporary magit-diff when commiting
;; TODO: extend help-mode behavior for other buffers (e.g. Occur)

(defvar pick-window-keys
  '(("q" . "Q") ("a" . "A") ("z" . "Z") ("x" . "X") ("s" . "S") ("w" . "W")
    ("e" . "E") ("d" . "D") ("c" . "C") ("v" . "V") ("f" . "F") ("r" . "R")
    ("t" . "T") ("g" . "G") ("b" . "B") ("n" . "N") ("h" . "H") ("y" . "Y")
    ("u" . "U") ("j" . "J") ("m" . "M") ("k" . "K") ("i" . "I") ("l" . "L")
    ("o" . "O") ("p" . "P"))
  "An alist of keys, where each element's car is a key that chooses the window,
while the cdr is the key that splits it and chooses the created child window.")

(defun display-buffer-pick-window (buf alist)
  (pick-window--log "buf:" nil
                    ("%S " buf) 'font-lock-keyword-face
                    "mode:" nil
                    ("%S " (buffer-local-value 'major-mode buf)) 'font-lock-string-face
                    "alist:" nil
                    ("%S " alist) 'font-lock-comment-face
                    "visible-buffers:" nil
                    ("%S" (window-list)) 'font-lock-doc-face)
  (when (and (cdr (window-list))
             (not (cdr (assq 'side alist))))
    (pick-window--log "picking window for " nil
                      ("%S" buf) 'font-lock-keyword-face)
    (when (eq this-command 'isearch-occur)
      (pick-window--disable-isearch))
    (when-let* ((w (pick-window t)))
      (window--display-buffer buf w 'window alist))))

(defun pick-window (&optional allow-split)
  (let* ((old-mode-line (default-value 'mode-line-format))
         (map (make-sparse-keymap))
         (bindings (-zip-pair pick-window-keys (window-list)))
         chosen-window split-p
         (handler
          (lambda ()
            (interactive)
            (let ((command-key (key-description (this-command-keys-vector))))
              (mapc (-lambda (((select-key . split-key) . window))
                      (if (cond ((string= command-key select-key)
                                 (setq chosen-window window))
                                ((and allow-split (string= command-key split-key))
                                 (setq chosen-window window
                                       split-p t)))
                          (exit-minibuffer)))
                    bindings)))))
    (set-keymap-parent map minibuffer-local-map)
    (mapc (-lambda (((select-key . split-key) . window))
            (keymap-set map select-key handler)
            (if allow-split
                (keymap-set map split-key handler))
            (set-window-parameter window 'pick-window-key-select select-key)
            (set-window-parameter window 'pick-window-key-split (and allow-split split-key)))
          bindings)
    (define-key map [remap self-insert-command] 'pick-window--unset-key-handler)
    (set-default 'mode-line-format
                 `(,(car old-mode-line)
                   (:eval (pick-window--mode-line))
                   ,@(cdr old-mode-line)))
    (unwind-protect
        (progn
          (read-from-minibuffer "Pick window: " nil map nil t)
          (if split-p
              (if (window-left-child (window-parent chosen-window))
                  (split-window-below nil chosen-window)
                (split-window-right nil chosen-window))
            chosen-window))
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
  (interactive (append (force-use-region) (list (pick-window))))
  (insert-to-window beg end window))

(defun cut-to-window (beg end window)
  (interactive (append (force-use-region) (list (pick-window))))
  (insert-to-window beg end window t))

(defun move-buffer-to-window (window)
  (interactive (list (pick-window t)))
  (let ((buf (current-buffer))
        (orig-pos (point)))
    (previous-buffer)
    (select-window window)
    (switch-to-buffer buf)
    (goto-char orig-pos)))

(defun swap-buffers-window (window)
  (interactive (list (pick-window)))
  (let ((this-buffer (current-buffer)))
    (switch-to-buffer (window-buffer window) nil t)
    (select-window window)
    (switch-to-buffer this-buffer nil t)))

(defun find-file-in-window (window)
  (interactive (list (pick-window t)))
  (let ((cwd (buffer-working-directory)))
    (select-window window)
    (let ((default-directory cwd))
      (ido-find-file))))

(defun insert-cwd-to-window (window)
  (interactive (list (pick-window t)))
  (let ((wd default-directory))
    (select-window window)
    (if (derived-mode-p 'term-mode)
        (term-send-raw-string wd)
      (insert wd))))


(defvar pick-window--disabled-modes
  '(debugger-mode xref--xref-buffer-mode))

(defun pick-window--match (buf &rest args)
  (with-current-buffer buf
    (not (or
          (string= (buffer-name) "*Completions*")
          (derived-mode-p pick-window--disabled-modes)
          (and (derived-mode-p 'help-mode)
               (memq (get-buffer buf)
                     (mapcar 'window-buffer (window-list))))))))

(defun pick-window--disable-isearch ()
  (mapc (lambda (b)
          (with-current-buffer b
            (when isearch-mode
              (pick-window--log "pick-window: disabling isearch-mode in %S" b)
              (isearch-done)
              (isearch-clean-overlays))))
        (buffer-list)))

(defun pick-window--mode-line ()
  (let ((select-key (window-parameter nil 'pick-window-key-select))
        (split-key (window-parameter nil 'pick-window-key-split)))
    (if select-key
        (concat
         (string-fontify "select:" 'font-lock-comment-face)
         (string-fontify select-key 'font-lock-function-name-face)
         (if split-key
             (concat
              " "
              (string-fontify "split:" 'font-lock-comment-face)
              (string-fontify split-key 'font-lock-keyword-face)))))))

(defun pick-window--unset-key-handler ()
  (interactive)
  (user-error "no window under key %S"
              (key-description (this-single-command-keys))))

(defmacro pick-window--log (&rest args)
  `(with-current-buffer (get-buffer-create "*pick-window-log*")
     (goto-char (point-max))
     (insert (format-fontify
              ("[%s] " (format-time-string "%Y-%m-%d %H:%M:%S")) 'font-lock-doc-face
              ,@args
              "\n"))))

(setq display-buffer-alist '((pick-window--match . (display-buffer-pick-window))))

(keymap-global-set "C-c c" 'copy-to-window)
(keymap-global-set "C-c x" 'cut-to-window)
(keymap-global-set "C-c b" 'move-buffer-to-window)
(keymap-global-set "C-c s" 'swap-buffers-window)
(keymap-global-set "C-c f" 'find-file-in-window)
(keymap-global-set "C-c D" 'insert-cwd-to-window)
