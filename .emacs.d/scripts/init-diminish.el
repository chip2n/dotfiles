(provide 'init-diminish)

(use-package diminish
  :ensure t
  :after (ivy projectile evil-snipe evil-lispy)
  :config
  (diminish 'undo-tree-mode)
  (diminish 'company-mode)
  (diminish 'counsel-mode)
  (diminish 'ivy-mode)
  (diminish 'projectile-mode)
  (diminish 'projectile-mode)
  (diminish 'evil-snipe-mode)
  (diminish 'evil-snipe-local-mode)
  (diminish 'lispy-mode)
  (diminish 'evil-lispy-mode)
  (diminish 'auto-revert-mode "arev")
  (diminish 'eldoc-mode)
  (diminish 'yas-minor-mode)
  (diminish 'evil-org-mode)
  (diminish 'org-indent-mode "indent"))
