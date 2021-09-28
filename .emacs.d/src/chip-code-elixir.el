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

(pretty-hydra-define hydra-elixir (:color teal :title "λ Elixir" :quit-key "q")
  ("Action"
   (("r" c/elixir-run-phx "mix phx.server")
    ("d" c/elixir-fetch-deps "mix deps.get")
    ("m" c/elixir-ecto-migrate "mix ecto.migrate"))

   "Testing"
   (("tc" exunit-verify "test current file")
    ("tt" exunit-rerun "rerun last test")
    ("tp" exunit-verify-all "test project"))))

(use-package elixir-mode
  :config
  (setq lsp-clients-elixir-server-executable '("~/dev/elixir-ls/release/language_server.sh"))
  (add-hook 'elixir-mode-hook 'lsp))

(use-package smartparens
  :config
  (require 'smartparens-config)
  (add-hook 'elixir-mode-hook 'smartparens-mode))

(use-package mix)

(use-package exunit
  :config
  (add-hook 'elixir-mode-hook 'exunit-mode))

(use-package inf-elixir
  :config
  (add-hook 'elixir-mode-hook 'inf-elixir-minor-mode))

(use-package web-mode
  :config
  (setq web-mode-markup-indent-offset 2))

(use-package polymode
  :after (web-mode polymode)
  :mode ("\.ex$" . poly-elixir-web-mode)
  :config
  (define-hostmode poly-elixir-hostmode :mode 'elixir-mode)
  (define-innermode poly-liveview-expr-elixir-innermode
    :mode 'web-mode
    :head-matcher (rx line-start (* space) "~H" (= 3 (char "\"'")) "\n")
    :tail-matcher (rx line-start (* space) (= 3 (char "\"'")) line-end)
    :head-mode 'host
    :tail-mode 'host
    :allow-nested nil
    :keep-in-mode 'host
    :fallback-mode 'host)
  (define-polymode poly-elixir-web-mode
    :hostmode 'poly-elixir-hostmode
    :innermodes '(poly-liveview-expr-elixir-innermode))
  ;; We don't care about the keybindings
  (setcdr polymode-mode-map nil))
(setq web-mode-engines-alist '(("elixir" . "\\.ex\\'")))

(provide 'chip-code-elixir)

;;; chip-code-elixir.el ends here