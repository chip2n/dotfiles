(provide 'init-ivy)

(use-package ivy
  :ensure t
  :config
  (ivy-mode)
  ;; slim down ivy display
  (setq ivy-count-format ""
        ivy-display-style nil
        ivy-minibuffer-faces nil))

(use-package counsel
  :ensure t
  :after (ivy)
  :config
  (counsel-mode))

;; more intelligent order when using counsel-M-x
(use-package smex
  :ensure t
  :config
  :after (counsel)
  (smex-initialize))

(use-package swiper
  :ensure t
  :after (ivy))

(use-package ivy-hydra
  :ensure t
  :after (ivy hydra))
