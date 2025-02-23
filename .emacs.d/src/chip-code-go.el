;;; chip-code-go.el -*- lexical-binding: t -*-

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

(defun c/go-run ()
  (interactive)
  (compile "go run ."))

(defun c/go-build ()
  (interactive)
  (compile "go build ."))

(defun c/go-test ()
  (interactive)
  (compile "go test"))

(use-package go-mode
  :hook (go-mode . eglot-ensure)
  :bind (:map go-mode-map
         ("C-c C-r" . c/go-run)
         ("C-c C-c" . c/go-build)
         ("C-c C-k" . kill-compilation)
         ("C-c C-t" . c/go-test)))

;; (use-package go-ts-mode
;;   :config
;;   (setq go-ts-mode-indent-offset 4))
;; (use-package templ-ts-mode)

(provide 'chip-code-go)

;;; chip-code-go.el ends here
