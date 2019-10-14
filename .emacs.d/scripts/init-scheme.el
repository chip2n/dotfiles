(use-package geiser
  :ensure t
  :config
  (setq geiser-chicken-binary "chicken-csi"))

(use-package racket-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist (cons (rx ".rkt" eos) 'racket-mode)))

(provide 'init-scheme)
