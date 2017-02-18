(provide 'clojure)

;; clojure-mode: https://github.com/clojure-emacs/clojure-mode
(use-package clojure-mode
  :ensure t)

;; cider
(use-package cider
  :ensure t)
(add-hook 'clojure-mode-hook 'cider-mode)

;; company-mode: https://github.com/company-mode/company-mode
(use-package company
  :ensure t)
(global-company-mode)
(add-hook 'cider-repl-mode-hook #'company-mode)
(add-hook 'cider-mode-hook #'company-mode)
