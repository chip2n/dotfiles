(provide 'init-projectile)

(use-package projectile
  :ensure t
  :after (ivy)
  :config
  (add-to-list 'projectile-globally-ignored-directories "*node_modules")
  (setq projectile-enable-caching t)
  ;; we remove -o flag so that untracked files are not included
  ;; this is mainly so that they don't always appear as the first search
  (setq projectile-git-command "git ls-files -zc --exclude-standard")
  (setq projectile-indexing-method 'alien)
  (setq projectile-completion-system 'ivy)
  (projectile-register-project-type 'shadow-cljs '("shadow-cljs.edn")
                                    :src-dir "src/main"
                                    :test-dir "src/test"
                                    :test-suffix "_test")
  (projectile-mode))

(use-package counsel-projectile
  :ensure t
  :after (counsel projectile))

;; For projectile-ag
(use-package ag
  :ensure t
  :after (projectile))
