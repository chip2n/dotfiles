;;; init.el --- My configuration for Emacs  -*- lexical-binding: t -*-

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

;; (eval-when-compile
;;   (add-to-list 'load-path "use-package")
;;   (require 'use-package))

(defvar bootstrap-version)

(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;;; Source paths

(add-to-list 'load-path "~/.emacs.d/new")
(add-to-list 'load-path "~/.emacs.d/scripts")

;;; Deferred compilation

(setq comp-deferred-compilation nil)

(defun chip/native-compile ()
  (interactive)
  (native-compile-async "~/.emacs.d" 'recursively))

;;; Desktop initialization

(require 'chip-init-desktop)
;; (require 'chip-init-android)

;;; init.el ends here
