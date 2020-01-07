(provide 'init-avy)

(use-package avy
  :ensure t)

(use-package ace-window
  :ensure t
  :config
  (setq aw-dispatch-always t))
