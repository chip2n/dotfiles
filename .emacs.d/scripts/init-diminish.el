(provide 'init-diminish)

(use-package diminish
  :ensure t
  :after (ivy projectile evil-snipe)
  :config
  (diminish 'undo-tree-mode)
  (diminish 'company-mode)
  (diminish 'counsel-mode)
  (diminish 'ivy-mode)
  (diminish 'projectile-mode))
