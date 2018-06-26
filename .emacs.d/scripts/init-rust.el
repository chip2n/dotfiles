(provide 'init-rust)

(use-package rust-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))
  (add-hook 'rust-mode-hook 'chip/setup-rust-keys))

(defun chip/setup-rust-keys ()
  "Setup keybindings for rust hacking"
  (interactive)
  (general-define-key
   :states 'normal
   :keymaps 'rust-mode-map
   "gd" 'racer-find-definition))

(use-package flymake-rust
  :ensure t
  :after rust-mode)

(use-package racer
  :ensure t
  :after rust-mode
  :config
  (add-hook 'rust-mode-hook #'racer-mode)
  (add-hook 'racer-mode-hook #'eldoc-mode)
  (add-hook 'racer-mode-hook #'company-mode))

(use-package cargo
  :ensure t
  :after rust-mode
  :config
  (add-hook 'rust-mode-hook 'cargo-minor-mode))
