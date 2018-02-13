(require 'ac-c-headers)

(defun c-mode-hook-ac-c-headers ()
  (add-to-list 'ac-sources 'ac-source-c-headers)
  (add-to-list 'ac-sources 'ac-source-c-header-symbols))

(add-hook 'c-mode-hook 'c-mode-hook-ac-c-headers)
