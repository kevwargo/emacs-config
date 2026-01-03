;; -*- lexical-binding: t -*-

(require 'cl-seq)
(require 'ibuffer)
(require 'ibuf-ext)

(defvar ibuffer-filters-modified-files
  '((visiting-file . nil) (modified . nil)))

(defun ibuffer-dwim ()
  (interactive)
  (let ((ibuffer-hook '(ibuffer-dwim-hook)))
    (ibuffer)))

(defun ibuffer-dwim-hook ()
  (setq ibuffer-filtering-qualifiers
        (if (cl-find-if #'ibuffer--buf-modified (buffer-list))
            ibuffer-filters-modified-files))
  (ibuffer-update nil t))

(defun ibuffer-delete-marked-exit ()
  (interactive)
  (ibuffer-do-kill-on-deletion-marks)
  (kill-current-buffer))

(defun ibuffer-save-all-modified ()
  (interactive)
  (if-let ((modified-bufs (cl-remove-if-not #'ibuffer--buf-modified (buffer-list))))
      (when (y-or-n-p (format "Save the following buffers: %S?"
                              (mapcar #'buffer-name modified-bufs)))
        (dolist (b modified-bufs)
          (with-current-buffer b
            (save-buffer)))
        (kill-current-buffer))
    (message "No modified buffers")))

(defun ibuffer--buf-modified (b)
  (and (buffer-file-name b) (buffer-modified-p b)))

(keymap-set ibuffer-mode-map "q" 'kill-current-buffer)
(keymap-set ibuffer-mode-map "X" 'ibuffer-delete-marked-exit)
(keymap-set ibuffer-mode-map "C-x C-s" 'ibuffer-save-all-modified)

(keymap-global-set "C-x C-b" 'ibuffer-dwim)
