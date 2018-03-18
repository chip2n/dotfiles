(provide 'init-clojure)

(use-package clojure-mode
  :ensure t
  :config
  (setq show-paren-delay 0)
  (add-hook 'clojure-mode-hook (show-paren-mode 1)))
