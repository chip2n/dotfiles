;;; chip-code-flutter.el -*- lexical-binding: t -*-

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

(use-package lsp-dart
  :defer t
  :config
  (setq lsp-dart-line-length 120)
  (setq lsp-dart-sdk-dir "~/flutter/bin/cache/dart-sdk")
  (setq lsp-dart-flutter-sdk-dir "~/flutter")
  (add-hook 'dart-mode-hook 'lsp)
  (setq lsp-dart-dap-flutter-hot-reload-on-save t)
  (setq lsp-dart-flutter-widget-guides nil))

(use-package dart-mode
  :after (projectile)
  :defer t
  :config
  (add-to-list 'auto-mode-alist (cons (rx ".dart" eos) 'dart-mode))
  ;; (add-hook 'dart-mode-hook 'flycheck-mode)
  (after-load (outshine-mode)
    (add-hook 'dart-mode-hook 'outshine-mode))
  (add-to-list 'projectile-project-root-files-bottom-up "pubspec.yaml")
  (add-to-list 'projectile-project-root-files-bottom-up "BUILD")

  (general-define-key
   :states 'normal
   :keymaps 'dart-mode-map
   "gd" 'dart-server-goto)

  (general-define-key
   :prefix "C-c"
   :states 'normal
   :keymaps 'dart-mode-map
   "f" 'dart-server-format))

(defun flutter--find-project-root ()
  (locate-dominating-file (buffer-file-name) "pubspec.yaml"))

(use-package flutter
  ;; :hook (dart-mode . (lambda () (add-hook 'after-save-hook #'flutter-run-or-hot-reload nil t)))
  :bind (:map dart-mode-map
         ("C-c c" . flutter-run-or-hot-reload))
  :config
  (setq flutter-sdk-path "~/flutter"))

(defun c/flutter-build-runner ()
  (interactive)
  (let* ((program "dart")
         (buffer (get-buffer-create "*build-runner*"))
         (proc-alive (comint-check-proc buffer))
         (process (get-buffer-process buffer)))
    ;; if the process is dead then re-create the process and reset the
    ;; mode.
    (unless proc-alive
      (with-current-buffer buffer
        (apply 'make-comint-in-buffer "Build runner" buffer
               program nil '("run" "build_runner" "watch"))
        ;; (cassandra-mode)
        ))
    ;; Regardless, provided we have a valid buffer, we pop to it.
    (when buffer
      (pop-to-buffer buffer))))

(provide 'chip-code-flutter)

;;; chip-code-flutter.el ends here
