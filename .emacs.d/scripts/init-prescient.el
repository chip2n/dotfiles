;;; Prescient allows you to filter and automatically sort ivy and company results
;;; by frequency. It also enables searching by initialism (e.g. stbow ->
;;; switch-to-buffor-other-window).

(use-package prescient
  :ensure t
  :after (counsel)
  :config
  (prescient-persist-mode))

(use-package ivy-prescient
  :ensure t
  :after (counsel)
  :config
  (ivy-prescient-mode))

(use-package company-prescient
  :ensure t
  :after (counsel)
  :config
  (company-prescient-mode))

(provide 'init-prescient)
