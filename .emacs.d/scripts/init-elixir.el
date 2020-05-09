(defvar lsp-elixir--config-options (make-hash-table))

(use-package elixir-mode
  :ensure t
  :config
  (add-to-list 'exec-path "/home/chip/elixir-ls/release")
  (add-hook 'elixir-mode-hook 'lsp)
  (add-hook 'lsp-after-initialize-hook
            (lambda () (lsp--set-configuration `(:elixirLS ,lsp-elixir--config-options)))))

(use-package exunit
  :ensure t)

(use-package inf-elixir
  :load-path "packages/inf-elixir/"
  :bind (
    ("C-c C-l i i" . 'inf-elixir)
    ("C-c C-l i p" . 'inf-elixir-project)
    ("C-c C-l i l" . 'inf-elixir-send-line)
    ("C-c C-l i r" . 'inf-elixir-send-region)
    ("C-c C-l i b" . 'inf-elixir-send-buffer)))

(provide 'init-elixir)
