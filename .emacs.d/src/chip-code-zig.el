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

(defun c/zig--steps ()
  "Get list of all available build steps."
  (->> (with-temp-buffer
         (insert (shell-command-to-string "zig build --help"))
         (beginning-of-buffer)
         (search-forward "Steps:")
         (delete-region (point-min) (point))
         (next-line)
         (re-search-forward "^[[:space:]]*$")
         (delete-region (point) (point-max))
         (buffer-string))
       (s-trim)
       (s-lines)
       (-map (lambda (line) (car (s-split " " (s-trim line)))))))

(defun c/zig-build ()
  (interactive)
  (let ((default-directory (projectile-locate-dominating-file (buffer-file-name) "build.zig")))
    (let ((step (completing-read "Select build step:" (c/zig--steps))))
      (zig--run-cmd (format "build %s" step)))))

(defun c/zig-clean ()
  (interactive)
  (let ((root (projectile-locate-dominating-file (buffer-file-name) "build.zig")))
    (delete-directory (expand-file-name "zig-out" root) t)
    (delete-directory (expand-file-name "zig-cache" root) t)))

(use-package zig-mode
  ;; :after (lsp-mode)
  :bind (:map zig-mode-map
         ("C-c C-r" . c/zig-compile-run)
         ("C-c C-b" . c/zig-build)
         ("C-c C-k" . kill-compilation)
         ("C-c C-t" . c/zig-test)
         ("C-c C-d" . c/zig-test-this))
  ;; :hook ((zig-mode . electric-pair-local-mode))
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

(defun c/zig--locate-root ()
  (locate-dominating-file default-directory "build.zig"))

(defun c/zig-compile ()
  "Compile using `zig build`."
  (interactive)
  (let ((default-directory (c/zig--locate-root)))
    (zig--run-cmd c/zig-build-cmd)))

(defun c/zig-compile-run ()
  "Compile and run using `zig build run`."
  (interactive)
  (let ((default-directory (c/zig--locate-root)))
    (zig--run-cmd "build" nil "run")))

(defun c/zig-test ()
  "Test using `zig build test`."
  (interactive)
  (let ((default-directory (c/zig--locate-root)))
    (zig--run-cmd "build test")))

(defun c/zig-test-this ()
  "Test current file using `zig test`."
  (interactive)
  (let ((default-directory (c/zig--locate-root))
        (path (buffer-file-name)))
    (zig--run-cmd "test -lc" path)))

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
