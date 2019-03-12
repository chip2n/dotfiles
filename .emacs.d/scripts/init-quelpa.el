(use-package quelpa
  :ensure t
  :config
  (setq quelpa-checkout-melpa-p nil)    ; we're not using it for MELPA packages
  (quelpa '(tayl :repo "chip2n/tayl.el" :fetcher github)))

(provide 'init-quelpa)
