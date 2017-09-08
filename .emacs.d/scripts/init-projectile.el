(provide 'init-projectile)

(use-package projectile
  :ensure t)
(projectile-mode)
(use-package helm-projectile
  :ensure t
  :init
  (defvar helm-source-file-not-found
    (helm-build-dummy-source
	"Create file"
      :action 'find-file))
  (add-to-list 'helm-projectile-sources-list helm-source-file-not-found t)
  (add-to-list 'projectile-globally-ignored-directories "*node_modules")
  (setq projectile-enable-caching t))
(evil-leader/set-key
  "p" 'helm-projectile
  "P" 'helm-projectile-switch-project)
