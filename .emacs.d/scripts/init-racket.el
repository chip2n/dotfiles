(use-package racket-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist (cons (rx ".rkt" eos) 'racket-mode)))

(use-package geiser
  :ensure t
  :config
  ;; (add-to-list 'auto-mode-alist (cons (rx ".rkt" eos) 'geiser-mode))
  )

(provide 'init-racket)
