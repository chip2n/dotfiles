(provide 'init-lispy)

(use-package lispy
  :ensure t
  :config
  ;; (add-hook 'emacs-lisp-mode-hook #'lispy-mode)
  ;; (add-hook 'clojure-mode-hook #'lispy-mode)
  ;; (add-hook 'clojurescript-mode-hook #'lispy-mode)
  )

;; (use-package lispyville
;;   :ensure t
;;   :after lispy
;;   :config
;;   (add-hook 'lispy-mode-hook #'lispyville-mode))

(use-package evil-lispy
  :ensure t
  :after lispy
  :config
  (add-hook 'emacs-lisp-mode-hook #'evil-lispy-mode)
  (add-hook 'clojure-mode-hook #'evil-lispy-mode)
  (add-hook 'clojurescript-mode-hook #'evil-lispy-mode)
  (add-hook 'slime-mode-hook #'evil-lispy-mode))
