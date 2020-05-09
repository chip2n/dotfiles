(provide 'init-rust)

(use-package rustic
  :ensure t
  :config
  ;; (setq rustic-lsp-server 'rust-analyzer)
  (setq rustic-lsp-server 'rls)
  )

;; (use-package eglot
;;   :ensure t)

;; (use-package rust-mode
;;   :ensure t
;;   :after company
;;   :hook (rust-mode . eglot-ensure)
;;   :config
;;   (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))
;;   (add-hook 'rust-mode-hook 'electric-pair-mode)
;;   (add-hook 'rust-mode-hook 'company-mode))

;; (defun chip/setup-rust-keys ()
;;   "Setup keybindings for rust hacking"
;;   (interactive)
;;   (general-define-key
;;    :states 'normal
;;    :keymaps 'rust-mode-map
;;    "gd" 'racer-find-definition))

;; (use-package flycheck-rust
;;   :ensure t
;;   :after rust-mode
;;   :config (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

;; (use-package cargo
;;   :ensure t
;;   :after rust-mode
;;   :config
;;   (add-hook 'rust-mode-hook 'cargo-minor-mode))
