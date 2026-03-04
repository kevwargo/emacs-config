;; -*- lexical-binding: t -*-

(require 'term)
(require 'dash)

(defvar pick-window-keys
  '("q" "a" "z" "x" "s" "w"
    "e" "d" "c" "v" "f" "r"
    "t" "g" "b" "n" "h" "y"
    "u" "j" "m" "k" "i" "l"
    "o" "p"))

(defvar pick-window-split-key "/")

(defvar pick-window--disabled-modes
  '(debugger-mode xref--xref-buffer-mode))

(defun pick-window--match (buf &rest args)
  (setq buf (get-buffer buf))
  (with-current-buffer buf
    (not (or
          (string= (buffer-name) "*Completions*")
          (derived-mode-p pick-window--disabled-modes)
          (and (derived-mode-p 'help-mode)
               (memq buf (mapcar 'window-buffer (window-list))))))))

(defun pick-window--disable-isearch ()
  (mapc (lambda (b)
          (with-current-buffer b
            (when isearch-mode
              (message "pick-window: disabling isearch-mode in %S" b)
              (isearch-done)
              (isearch-clean-overlays))))
        (buffer-list)))

(defun display-buffer-pick-window (buf alist)
  (when (and (cdr (window-list))
             (not (cdr (assq 'side alist))))
    (when (eq this-command 'isearch-occur)
      (pick-window--disable-isearch))
    (when-let* ((w (pick-window t)))
      (window--display-buffer buf w 'window alist))))

(defvar pick-window--split-p nil)

(defun pick-window--toggle-split ()
  (interactive)
  (setq pick-window--split-p (null pick-window--split-p))
  (force-mode-line-update t))

(defun pick-window--mode-line (allow-split)
  (when-let* ((key (window-parameter (selected-window) 'pick-window-key)))
    (concat
     (string-fontify (if pick-window--split-p "split:" "select:") 'font-lock-comment-face)
     (string-fontify key 'font-lock-function-name-face)
     (if allow-split
         (concat
          (string-fontify " toggle-split:" 'font-lock-comment-face)
          (string-fontify pick-window-split-key 'font-lock-keyword-face))
       ""))))

(defun pick-window (&optional allow-split)
  (let ((old-mode-line (default-value 'mode-line-format))
        (map (make-sparse-keymap))
        chosen-window)
    (set-keymap-parent map minibuffer-local-map)
    (if allow-split
        (keymap-set map pick-window-split-key 'pick-window--toggle-split))
    (mapc (-lambda ((key . window))
            (set-window-parameter window 'pick-window-key key)
            (keymap-set map key
                        (lambda ()
                          (interactive)
                          (setq chosen-window window)
                          (exit-minibuffer))))
          (-zip-pair pick-window-keys (window-list)))
    (define-key map [remap self-insert-command]
                (lambda ()
                  (interactive)
                  (user-error "no window under key %S"
                              (key-description (this-single-command-keys)))))
    (set-default 'mode-line-format `(,(car old-mode-line)
                                     (:eval (pick-window--mode-line ,allow-split))
                                     ,@(cdr old-mode-line)))
    (setq pick-window--split-p nil)
    (unwind-protect
        (progn
          (read-from-minibuffer "Pick window: " nil map nil t)
          (if pick-window--split-p
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

(setq display-buffer-alist '((pick-window--match . (display-buffer-pick-window))))

(keymap-global-set "C-c c" 'copy-to-window)
(keymap-global-set "C-c x" 'cut-to-window)
(keymap-global-set "C-c b" 'move-buffer-to-window)
(keymap-global-set "C-c s" 'swap-buffers-window)
(keymap-global-set "C-c f" 'find-file-in-window)
(keymap-global-set "C-c D" 'insert-cwd-to-window)
