(require 'ajc-java-complete-config)

(defun jde-add-1.7 ()
  (defclass jde-compile-javac-17 (jde-compile-javac-16)
    ()
    "Class of JDK 1.7 javac compilers.")

  (defmethod initialize-instance ((this jde-compile-javac-17) &rest fields)
    ;; Call parent initializer.

    (call-next-method)

    ;; Set compiler version.
    (oset this version "1.7"))
  (setq jde-compile-javac-compilers
        (list
         (jde-compile-javac-11 "javac 1.1.x")
         (jde-compile-javac-12 "javac 1.2.x")
         (jde-compile-javac-13 "javac 1.3.x")
         (jde-compile-javac-14 "javac 1.4.x")
         (jde-compile-javac-15 "javac 1.5.x")
         (jde-compile-javac-16 "javac 1.6.x")
         (jde-compile-javac-17 "javac 1.7.x"))))

(defun jde-keymap-modify ()
  (local-set-key (kbd "C-c C-v g") 'jde-wiz-get-set-methods)
  (local-set-key (kbd "C-<left>") 'jde-beginning-of-camel-tok)
  (local-set-key (kbd "C-<right>") 'jde-end-of-camel-tok)
  (local-set-key (kbd "C-c r") 'jde-compile&run))

(defun jde-compile&run ()
  "Compile and if compilation succeeded, run the application."
  ; In this function we execute `jde-compile' and define hook to execute `jde-run'
  ; after it's finished
  (interactive)
  ; I know it's a dirty hack, but I don't give damn, I just love LISP :P
  ; Seriously that code will be executed in unknown moment of time and
  ; definitely not in any `let' we can define
  (defun _tmp-jde-compile-handler (buf msg)
    (when (zerop exit-status) ; `exit-status' is an argument passed
                              ; to function `compilation-handle-exit',
                              ; so don't worry it's not defined here :P
      (with-current-buffer _tmp-jde-src-buffer
        (jde-run 1)))
    (remove-hook 'jde-compile-finish-hook '_tmp-jde-compile-handler)
    (makunbound '_tmp-jde-src-buffer)
    (fmakunbound '_tmp-jde-compile-handler))
  (setq _tmp-jde-src-buffer (current-buffer))
  (add-hook 'jde-compile-finish-hook '_tmp-jde-compile-handler)
  (jde-compile))

(add-hook 'jde-mode-hook 'jde-add-1.7)
(add-hook 'jde-mode-hook 'jde-keymap-modify)
;; (add-hook 'jde-mode-hook 'ajc-java-complete-mode)
