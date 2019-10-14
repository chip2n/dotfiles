(provide 'init-lisp)

(use-package slime
  :ensure t
  :config
  (setq inferior-lisp-program "/usr/bin/sbcl")
  (add-hook 'slime-repl-mode-hook 'header-mode)
  (add-hook 'slime-macroexpansion-minor-mode-hook (lambda () (interactive) (evil-motion-state)))
  (slime-setup '(slime-fancy slime-asdf slime-cl-indent slime-company)))

(use-package slime-company
  :ensure t
  :after (slime company))
