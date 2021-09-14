;;; chip-code-elixir.el -*- lexical-binding: t -*-

;; Copyright (C) 2021  Andreas Arvidsson
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

(define-minor-mode c/code-elixir-mode
  "Mode for working with Elixir code."
  :lighter nil
  (if c/code-elixir-mode
      (progn
        (setq c/compilation-fun #'hydra-elixir/body)
        (setq prettify-symbols-alist '(("|>" . "▷")
                                       ("<-" . "←")
                                       ("->" . "→")
                                       ("=>" . "⇒")))
        (prettify-symbols-mode 1))
    (kill-local-variable c/compilation-fun)
    (prettify-symbols-mode -1)))

(add-hook 'elixir-mode-hook #'c/code-elixir-mode)

;; (general-define-key
;;  :keymaps 'c/code-elixir-mode-map
;;   "C-c c" 'hydra-elixir/body)

(defun c/elixir-run-phx ()
  (interactive)
  (inf-elixir-project "iex -S mix phx.server")
  ;; (with-dominating-file-dir "mix.exs"
  ;;   (async-shell-command "mix phx.server" "*phx.server*")
  ;;   ;; (compile "mix phx.server")
  ;;   )
  )

(defun c/elixir-fetch-deps ()
  (interactive)
  (with-dominating-file-dir "mix.exs"
    (async-shell-command "mix deps.get" "*mix deps.get*")))

(defun c/elixir-ecto-migrate ()
  (interactive)
  (with-dominating-file-dir "mix.exs"
    (async-shell-command "mix ecto.migrate" "*mix deps.get*")))

(defhydra hydra-elixir ()
  "elixir"
  ("r" c/elixir-run-phx "mix phx.server")
  ("d" c/elixir-fetch-deps "mix deps.get")
  ("m" c/elixir-ecto-migrate "mix ecto.migrate"))

(use-package elixir-mode
  :config
  (setq lsp-clients-elixir-server-executable '("~/dev/elixir-ls/release/language_server.sh"))
  (add-hook 'elixir-mode-hook 'lsp))

(use-package mix)

(use-package exunit
  :config
  (add-hook 'elixir-mode-hook 'exunit-mode))

(use-package inf-elixir
  :config
  (add-hook 'elixir-mode-hook 'inf-elixir-minor-mode))

(provide 'chip-code-elixir)

;;; chip-code-elixir.el ends here
