(require 'kotlin-mode)
(require 'reformatter)

(setq kotlin-tab-width 2)

;; https://github.com/facebook/ktfmt/releases/download/v0.63/ktfmt-0.63-with-dependencies.jar
(defvar kotlin-ktfmt-jar-path
  (expand-file-name "ktfmt-0.63-with-dependencies.jar"
                    user-emacs-directory))

(reformatter-define kotlin-ktfmt
  :program "java"
  :args (list "-jar" kotlin-ktfmt-jar-path
              (format "--stdin-name=%s" (file-name-nondirectory (buffer-file-name)))
              "-")
  :lighter " KtFmt")

(defun kotlin--hook ()
  (if (file-exists-p kotlin-ktfmt-jar-path)
      (kotlin-ktfmt-on-save-mode)))

(add-hook 'kotlin-mode-hook 'kotlin--hook)
