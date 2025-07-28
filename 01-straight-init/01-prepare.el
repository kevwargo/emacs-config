;; -*- lexical-binding: t -*-

(defun kev-straight--prepare-pkg (pkg-name &rest _)
  (if (string= pkg-name "lsp-mode")
      (setenv "LSP_USE_PLISTS" "true")))

(add-hook 'straight-use-package-prepare-functions #'kev-straight--prepare-pkg)
