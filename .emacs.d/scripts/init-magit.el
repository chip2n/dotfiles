(provide 'init-magit)

(use-package magit
  :ensure t)
(use-package evil-magit
  :ensure t
  :after magit)
(evil-leader/set-key
  "g" 'magit-status)
