(provide 'init-snipe)

(use-package evil-snipe
  :ensure t
  :config
  (setq evil-snipe-scope 'buffer)
  (evil-snipe-mode +1)
  (evil-snipe-override-mode +1)
  ;; turn off evil-snipe in ranger
  (add-hook 'ranger-mode-hook 'turn-off-evil-snipe-override-mode)
  ;; turn off evil-snipe in magit
  (add-hook 'magit-mode-hook 'turn-off-evil-snipe-override-mode))
