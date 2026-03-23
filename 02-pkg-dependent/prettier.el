(require 'prettier)

(add-hook 'js-mode-hook 'prettier-mode)
(add-hook 'typescript-ts-mode-hook 'prettier-mode)
(add-hook 'tsx-ts-mode-hook 'prettier-mode)
