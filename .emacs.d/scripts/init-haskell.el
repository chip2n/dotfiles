(provide 'init-haskell)

(use-package lsp-mode
  :ensure t)

(use-package lsp-ui
  :ensure t)

(use-package lsp-haskell
  :ensure t
  :after (lsp-mode lsp-ui)
  :config
  (setq lsp-haskell-process-path-hie "hie-wrapper")
  (setq lsp-ui-doc-max-width 80)
  (setq lsp-ui-doc-max-height 10)
  (setq lsp-ui-doc-use-childframe t)
  (add-hook 'lsp-mode-hook 'lsp-ui-mode)
  (add-hook 'haskell-mode-hook 'lsp-haskell-enable)
  (add-hook 'haskell-mode-hook 'flycheck-mode))
