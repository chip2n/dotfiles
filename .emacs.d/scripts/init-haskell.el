(provide 'init-haskell)

;; haskell-mode: https://github.com/haskell/haskell-mode
(use-package haskell-mode
  :ensure t)

(use-package intero
  :ensure t)
(add-hook 'haskell-mode-hook 'intero-mode)
