(provide 'init-jade)

(use-package pug-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist (cons (rx ".jade" eos) 'pug-mode))
  (add-to-list 'auto-mode-alist (cons (rx ".pug" eos) 'pug-mode)))

(use-package stylus-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist (cons (rx ".styl" eos) 'pug-mode)))
