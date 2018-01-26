(provide 'init-projectile)

(use-package projectile
  :ensure t
  :after (ivy)
  :config
  (add-to-list 'projectile-globally-ignored-directories "*node_modules")
  (setq projectile-enable-caching t)
  (setq projectile-completion-system 'ivy)
  (projectile-mode))

;; For projectile-ag
(use-package ag
  :ensure t
  :after (projectile))
