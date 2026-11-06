;; -*- lexical-binding: t; -*-

(require 'yasnippet)

(setq yas-snippet-dirs (list (expand-file-name "snippets" kec-config-dir)))

(keymap-unset yas-minor-mode-map "TAB")
(keymap-unset yas-minor-mode-map "<tab>")
(keymap-set yas-minor-mode-map "C-c TAB" 'yas-expand)
(keymap-set yas-minor-mode-map "C-c & l" 'yas-reload-all)

(add-hook 'find-file-hook 'yas-reload-all)

(yas-global-mode 1)
