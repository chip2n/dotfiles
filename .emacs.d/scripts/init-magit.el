(provide 'init-magit)

(use-package magit
  :ensure t
  :config
  (add-hook 'magit-mode-hook (lambda () (display-line-numbers-mode -1)))
  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)

  ;; start magit commit buffers in evil insert mode
  (add-hook 'git-commit-mode-hook 'evil-insert-state))

;; (use-package evil-magit
;;   :ensure t
;;   :after magit)

(use-package forge
  :ensure t
  :after magit)

(use-package ssh-agency
  :ensure t
  :config
  (setq ssh-agency-keys '("~/.ssh/github")))
