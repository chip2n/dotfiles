(provide 'init-haskell)

(use-package lsp-haskell
  :ensure t
  :after (lsp-mode lsp-ui)
  :config
  (setq lsp-haskell-process-path-hie "hie-wrapper")
  (add-hook 'haskell-mode-hook 'lsp-haskell-enable)
  (add-hook 'haskell-mode-hook 'flycheck-mode))
