(provide 'init-company)

(use-package company
  :ensure t
  :config
  (global-company-mode))

(use-package company-anaconda
  :ensure t
  :after (company anaconda-mode)
  :config
  (add-to-list 'company-backends 'company-anaconda))
