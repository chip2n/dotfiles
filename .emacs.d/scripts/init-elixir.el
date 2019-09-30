(defvar lsp-elixir--config-options (make-hash-table))

(use-package elixir-mode
  :ensure t
  :config
  (add-to-list 'exec-path "/home/chip/elixir-ls/release")
  (add-hook 'elixir-mode-hook 'lsp)
  (add-hook 'lsp-after-initialize-hook
            (lambda () (lsp--set-configuration `(:elixirLS ,lsp-elixir--config-options)))))

(provide 'init-elixir)
