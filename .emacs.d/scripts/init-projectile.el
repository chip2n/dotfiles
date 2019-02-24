(provide 'init-projectile)

(use-package projectile
  :ensure t
  :after (ivy)
  :config
  (add-to-list 'projectile-globally-ignored-directories "*node_modules")
  (setq projectile-enable-caching t)
  (setq projectile-completion-system 'ivy)
  (projectile-register-project-type 'shadow-cljs '("shadow-cljs.edn")
                                    :src-dir "src/main"
                                    :test-dir "src/test"
                                    :test-suffix "_test")
  (projectile-mode))

;; For projectile-ag
(use-package ag
  :ensure t
  :after (projectile))
