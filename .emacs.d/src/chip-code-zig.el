;;; chip-code-zig.el -*- lexical-binding: t -*-

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

(defun c/zig--setup ()
  ;; zig-mode sets this, but it forces each zig buffer to have `zig build` as a
  ;; default command, while I usually want to use the same command in multiple
  ;; buffers
  (kill-local-variable 'compile-command))

(use-package zig-mode
  ;; :after (lsp-mode)
  :bind (:map zig-mode-map
         ("C-c C-c" . c/smart-compile)
         ("C-c C-r" . c/smart-recompile)
         ("C-c C-k" . kill-compilation)
         ("C-c C-t" . c/zig-test))
  ;; :hook ((zig-mode . electric-pair-local-mode))
  :hook ((zig-mode . c/zig--setup))
  :config
  ;; formatting on save breaks lsp-mode
  ;; see https://github.com/ziglang/zig-mode/issues/49
  ;; (setq zig-format-on-save nil)
  (add-to-list 'lsp-language-id-configuration '(zig-mode . "zig"))
  (setq zig-format-on-save nil)
  ;; (add-hook 'zig-mode-hook 'lsp)
  (add-hook 'zig-mode-hook 'eglot-ensure)

  (after-load (outshine)
    (add-hook 'zig-mode-hook 'outshine-mode)))

(defun c/zig-clean ()
  (interactive)
  (let ((root (locate-dominating-file default-directory "build.zig")))
    (delete-directory (expand-file-name "zig-out" root) t)
    (delete-directory (expand-file-name ".zig-cache" root) t)))

(defun c/zig-compile ()
  (interactive)
  (let ((default-directory (locate-dominating-file default-directory "build.zig")))
    (call-interactively 'compile)))

(defun c/zig-recompile ()
  (interactive)
  (let ((default-directory (locate-dominating-file default-directory "build.zig")))
    (call-interactively 'recompile)))

(defun c/zig-test ()
  "Test using `zig build test`."
  (interactive)
  (let ((default-directory (locate-dominating-file default-directory "build.zig")))
    (compilation-start "zig build test")))

(defun c/zig-debug ()
  (interactive)
  (let ((existing (get-buffer "*gdb*")))
    (when existing (kill-buffer existing)))
  (let ((pid (shell-command-to-string "pgrep zig-dbg"))
        (buffer (save-window-excursion (vterm "*gdb*"))))
    (switch-to-buffer-other-window buffer)
    (vterm-send-string "gdb")
    (vterm-send-return)
    (vterm-send-string (format "attach %s" pid))))

(provide 'chip-code-zig)

;;; chip-code-zig.el ends here
