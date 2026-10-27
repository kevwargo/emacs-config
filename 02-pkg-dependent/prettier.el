;; -*- lexical-binding: t; -*-

(require 'prettier)

(add-hook 'js-mode-hook 'prettier-mode)
(add-hook 'typescript-ts-mode-hook 'prettier-mode)
(add-hook 'tsx-ts-mode-hook 'prettier-mode)
(add-hook 'html-mode-hook 'prettier-mode)
(add-hook 'css-mode-hook 'prettier-mode)
