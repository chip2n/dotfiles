(provide 'init-avy)

(use-package avy
  :ensure t)

(use-package ace-window
  :ensure t
  :config
  (setq aw-dispatch-always t)
  (ivy-add-actions
   'ivy-switch-buffer
   '(("a" ace-window "ace-window"))))
