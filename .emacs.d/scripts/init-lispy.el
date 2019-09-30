(provide 'init-lispy)

(use-package lispy
  :ensure t
  :config
  ;; (add-hook 'emacs-lisp-mode-hook #'lispy-mode)
  ;; (add-hook 'clojure-mode-hook #'lispy-mode)
  ;; (add-hook 'clojurescript-mode-hook #'lispy-mode)
  (setq lispy-close-quotes-at-end-p t))

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
  (add-hook 'cider-repl-mode-hook #'evil-lispy-mode)
  (add-hook 'clojurescript-mode-hook #'evil-lispy-mode)
  (add-hook 'slime-mode-hook #'evil-lispy-mode)
  (add-hook 'racket-mode-hook #'evil-lispy-mode)
  (add-hook 'slime-repl-mode-hook #'evil-lispy-mode)
  (add-hook 'scheme-mode-hook #'evil-lispy-mode))
