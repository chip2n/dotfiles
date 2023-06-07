(require 'heex-ts-mode)
(require 'elixir-ts-mode)

(setq lsp-clients-elixir-server-executable '("~/dev/elixir-ls/release/language_server.sh"))
(add-hook 'elixir-ts-mode-hook 'lsp)
(add-hook 'elixir-ts-mode-hook 'electric-pair-local-mode)

(define-minor-mode c/code-elixir-mode
  "Mode for working with Elixir code."
  :lighter nil
  (if c/code-elixir-mode
      (progn
        (setq prettify-symbols-alist '(("|>" . "▷")
                                       ("<-" . "←")
                                       ("->" . "→")
                                       ("=>" . "⇒")))
        (prettify-symbols-mode 1))
    (prettify-symbols-mode -1)))

(add-hook 'elixir-ts-mode-hook #'c/code-elixir-mode)

;;   (setq lsp-clients-elixir-server-executable '("~/dev/elixir-ls/release/language_server.sh"))
;;   (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\.elixir_ls\\'")

;; (use-package elixir-mode
;;   :after (lsp-mode)
;;   :defer t
;;   :config
;;   (setq lsp-clients-elixir-server-executable '("~/dev/elixir-ls/release/language_server.sh"))
;;   (add-hook 'elixir-mode-hook 'lsp)
;;   (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\.elixir_ls\\'"))

;; (use-package elixir-ts-mode
;;   :after (lsp-mode)
;;   :defer t
;;   :hook (elixir-ts-mode . lsp-mode)
;;   :config
;;   (cl-delete "\\.ex\\'" auto-mode-alist :test 'equal :key 'car)
;;   (setq lsp-clients-elixir-server-executable '("~/dev/elixir-ls/release/language_server.sh"))
;;   (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\.elixir_ls\\'")
;;   )

;; (defmacro define-elixir-sigil-innermode (name sigil)
;;   `(define-innermode ,name
;;      :mode 'elixir-ts-mode
;;      :head-matcher (rx line-start (* space) ,sigil (= 3 (char "\"'")) "\n")
;;      :tail-matcher (rx line-start (* space) (= 3 (char "\"'")) line-end)
;;      :head-mode 'host
;;      :tail-mode 'host
;;      :allow-nested nil
;;      :keep-in-mode 'host
;;      :fallback-mode 'host))

;; (use-package polymode
;;   :after (elixir-ts-mode)
;;   :defer t
;;   :mode ("\.ex$" . poly-elixir-ts-mode)
;;   :config
;;   (define-hostmode poly-elixir-hostmode :mode 'elixir-mode)
;;   (define-elixir-sigil-innermode poly-elixir-sigil-h-innermode "~H")
;;   (define-elixir-sigil-innermode poly-elixir-sigil-f-innermode "~F")
;;   (define-polymode poly-elixir-ts-mode
;;     :hostmode 'poly-elixir-hostmode
;;     :innermodes '(poly-elixir-sigil-h-innermode
;;                   poly-elixir-sigil-f-innermode))
;;   ;; We don't care about the keybindings
;;   (setcdr polymode-mode-map nil))


(provide 'chip-code-elixir2)
