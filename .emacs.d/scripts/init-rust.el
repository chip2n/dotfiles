(provide 'init-rust)

(use-package eglot
  :ensure t)

(use-package rust-mode
  :ensure t
  :after company
  :hook (rust-mode . eglot-ensure)
  :config
  (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))
  ;; (add-hook 'rust-mode-hook 'chip/setup-rust-keys)
  (add-hook 'rust-mode-hook 'electric-pair-mode)
  (add-hook 'rust-mode-hook 'company-mode))

(defun chip/setup-rust-keys ()
  "Setup keybindings for rust hacking"
  (interactive)
  (general-define-key
   :states 'normal
   :keymaps 'rust-mode-map
   "gd" 'racer-find-definition)
  ;; (general-define-key
  ;;  :prefix leader
  ;;  :states 'normal
  ;;  :mode 'cargo-minor-mode
  ;;  "mr" 'cargo-process-run
  ;;  "mc" 'cargo-process-check
  ;;  "mt" 'cargo-process-test)
  )

;; (use-package flymake-rust
;;   :ensure t
;;   :after rust-mode)

(use-package flycheck-rust
  :ensure t
  :after rust-mode
  :config (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

;; (use-package racer
;;   :ensure t
;;   :after rust-mode
;;   :config
;;   (add-hook 'rust-mode-hook #'racer-mode)
;;   (add-hook 'racer-mode-hook #'eldoc-mode)
;;   (add-hook 'racer-mode-hook #'company-mode))

(use-package cargo
  :ensure t
  :after rust-mode
  :config
  (add-hook 'rust-mode-hook 'cargo-minor-mode))
