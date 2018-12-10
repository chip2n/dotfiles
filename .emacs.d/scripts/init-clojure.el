(provide 'init-clojure)

(use-package clojure-mode
  :ensure t)

(use-package cider
  :ensure t
  :config
  (evil-collection-init 'cider))

(use-package inf-clojure
  :ensure t)
