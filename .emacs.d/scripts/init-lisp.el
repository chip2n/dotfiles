(provide 'init-lisp)

(use-package slime
  :ensure t
  :config
  (setq inferior-lisp-program "/usr/bin/sbcl")
  (setq slime-contribs '(slime-fancy))
  (add-hook 'slime-repl-mode-hook 'header-mode))
