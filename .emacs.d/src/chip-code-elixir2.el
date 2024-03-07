;;; chip-code-elixir.el -*- lexical-binding: t -*-

;; Copyright (C) 2022  Andreas Arvidsson
;;
;; Author: Andreas Arvidsson <andreas@arvidsson.io>
;; Keywords: config
;;
;; This file is not part of GNU Emacs
;;
;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Code:

(require 'eglot)

(use-package elixir-ts-mode
  :bind ("C-c C-f" . eglot-format-buffer)
  :hook (elixir-ts-mode . eglot-ensure)
  )

;; ;; This is optional. It automatically runs `M-x eglot` for you whenever you are in `elixir-mode`:
;; (add-hook 'elixir-mode-hook 'eglot-ensure)

;; Be sure to edit the path appropriately; use the `.bat` script instead for Windows:
(add-to-list 'eglot-server-programs '(elixir-ts-mode "/home/chip/dev/elixir-ls/release/language_server.sh"))

;; (require 'heex-ts-mode)
;; (require 'elixir-ts-mode)

;; (setq lsp-clients-elixir-server-executable '("~/dev/elixir-ls/release/language_server.sh"))
;; (add-hook 'elixir-ts-mode-hook 'lsp)
;; (add-hook 'elixir-ts-mode-hook 'electric-pair-local-mode)
;; (add-to-list 'auto-mode-alist '("\\.heex\\'" . elixir-ts-mode))

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

(use-package inf-elixir
  :defer t
  :config
  (add-hook 'elixir-mode-hook 'inf-elixir-minor-mode))

(defun c/elixir-run-phx ()
  (interactive)
  (inf-elixir-project "iex -S mix phx.server"))


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
