(require 'yasnippet)

(yas-global-mode 1)

(keymap-unset yas-minor-mode-map "TAB")
(keymap-unset yas-minor-mode-map "<tab>")
(keymap-set yas-minor-mode-map "C-c TAB" yas-maybe-expand)
(keymap-set yas-minor-mode-map "C-c & l" 'yas-reload-all)

(add-hook 'find-file-hook 'yas-reload-all)
