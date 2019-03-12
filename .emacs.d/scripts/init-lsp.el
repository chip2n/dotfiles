(use-package lsp-mode
  :ensure t
  :config
  (setq lsp-prefer-flymake nil)
  (setq lsp-eldoc-enable-hover nil)
  (setq lsp-enable-xref nil)
  (setq lsp-enable-completion-at-point nil)
  (setq lsp-ui-sideline-enable nil)
  (setq lsp-auto-guess-root t))

(use-package company-lsp
  :ensure t
  :after '(company)
  :config
  (setq company-lsp-cache-candidates t
        company-lsp-filter-candidates t))

;; (use-package lsp-ui
;;   :ensure t
;;   :config
;;   (setq lsp-ui-doc-max-width 80)
;;   (setq lsp-ui-doc-max-height 10)
;;   (setq lsp-ui-doc-use-childframe t)
;;   (add-hook 'lsp-mode-hook 'lsp-ui-mode))

(provide 'init-lsp)
