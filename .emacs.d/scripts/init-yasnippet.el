(provide 'init-yasnippet)

;; yasnippet
(use-package yasnippet
  :ensure t
  :init
  :config
  (yas-global-mode 1)
  (add-to-list 'yas-snippet-dirs (locate-user-emacs-file "snippets")))
