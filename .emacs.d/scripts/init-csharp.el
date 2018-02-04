(provide 'init-csharp)

(use-package omnisharp
  :ensure t
  :after (company)
  :config
  (add-to-list 'company-backends 'company-omnisharp)
  (add-hook 'csharp-mode-hook #'company-mode)
  (add-hook 'csharp-mode-hook 'omnisharp-mode))
