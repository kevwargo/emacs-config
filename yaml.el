(require 'indent-tools)
(require 'yaml-mode)

(defun yaml-mode-customize ()
  (indent-tools-minor-mode 1)
  (modify-syntax-entry ?\( "()")
  (modify-syntax-entry ?\) ")("))

(defun yaml-fix-array-indentaion (beg end)
  (interactive "r")
  (save-excursion
    (goto-char beg)
    (while (< (point) end)
      (back-to-indentation)
      (when (eq (char-after (point)) ?-)
        (delete-horizontal-space)
        (indent-to (yaml-compute-indentation)))
      (forward-line 1))))

(add-hook 'yaml-mode-hook 'yaml-mode-customize)
