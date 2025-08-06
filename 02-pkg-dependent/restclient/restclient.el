(require 'restclient)

(defun restclient-json-format-body ()
  (interactive)
  (save-excursion
    (goto-char (restclient-current-max))
    (if (save-excursion
          (re-search-backward restclient-method-url-regexp (point-min) t))
        (json-pretty-print (save-excursion (backward-sexp) (point))
                           (point)))))

(keymap-set restclient-mode-map "C-c f" 'restclient-json-format-body)

(add-to-list 'auto-mode-alist '("\\.rest$" . restclient-mode))
