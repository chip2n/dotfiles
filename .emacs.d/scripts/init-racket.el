(use-package racket-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist (cons (rx ".rkt" eos) 'racket-mode)))

(use-package geiser
  :ensure t)

(provide 'init-racket)
