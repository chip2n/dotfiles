(provide 'init-magit)

(use-package magit
  :ensure t)
(use-package evil-magit
  :ensure t)
(evil-leader/set-key
  "g" 'magit-status)
