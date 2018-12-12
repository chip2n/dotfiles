(provide 'init-clojure)

(use-package clojure-mode
  :ensure t)

(use-package cider
  :ensure t
  :config
  (setq cider-test-show-report-on-success t)
  (add-to-list 'evil-motion-state-modes 'cider-test-report-mode)
  (evil-collection-init 'cider))

(use-package inf-clojure
  :ensure t)
