(provide 'init-evil)

(use-package evil
  :ensure t
  :init
  (setq evil-want-C-u-scroll t)
  :config
  (evil-mode 1))
  
(use-package evil-surround
  :ensure t
  :after (evil)
  :config
  (global-evil-surround-mode 1))
