;; -*- lexical-binding: t; -*-

(require 'kotlin-mode)
(require 'reformatter)

(setq kotlin-tab-width 2)

;; TODO: download semi-automatically from
;; https://github.com/facebook/ktfmt/releases/download/v0.63/ktfmt-0.63-with-dependencies.jar
(defvar kotlin-ktfmt-jar-path
  (expand-file-name "ktfmt-0.63-with-dependencies.jar"
                    user-emacs-directory))

(defun kotlin-android-docs-at-point ()
  "Open the Kotlin Android API documentation for the class imported at point.

The sexp at point is treated as the imported class name, e.g. `Handler'.
Searches the current buffer for an import corresponding to that name.
Only imports beginning with `android.' are considered."
  (interactive)
  (let* ((sexp (thing-at-point 'sexp t))
         (class-name (and sexp (string-trim sexp)))
         (import-regexp
          (and class-name
               (format "^\\s-*import\\s-+\\(android\\.[[:alnum:]_.]+\\.%s\\)\\s-*$"
                       (regexp-quote class-name)))))
    (unless class-name
      (user-error "No valid class name at point"))

    (save-excursion
      (goto-char (point-min))
      (if (re-search-forward import-regexp nil t)
          (let* ((fqcn (match-string-no-properties 1))
                 (path (replace-regexp-in-string "\\." "/" fqcn))
                 (url (format "https://developer.android.com/reference/kotlin/%s"
                              path)))
            (browse-url url))
        (user-error "No android import found for %s" class-name)))))

(reformatter-define kotlin-ktfmt
  :program "java"
  :args (list "-jar" kotlin-ktfmt-jar-path
              (format "--stdin-name=%s" (file-name-nondirectory (buffer-file-name)))
              "-")
  :lighter " KtFmt")

(defun kotlin--hook ()
  (keymap-local-set "C-{" 'embrace-selected-lines)
  (keymap-local-set "C-c C-." 'kotlin-android-docs-at-point)
  (if (file-exists-p kotlin-ktfmt-jar-path)
      (kotlin-ktfmt-on-save-mode)))

(add-hook 'kotlin-mode-hook 'kotlin--hook)
