;;; init.el --- My configuration for Emacs  -*- lexical-binding: t -*-

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

(defconst c/config-evil? nil
  "If non-nil, load evil-mode & related packages.")

(defconst c/config-meow? t
  "If non-nil, load meow-mode & related packages.")

(defconst chip-config-src-dir (concat user-emacs-directory "src/")
  "Path to emacs config src directory.")

(defconst chip-config-cache-dir (concat user-emacs-directory ".cache/"))

(defconst chip-dev-dir "~/dev/"
  "Path to development directory.")

(defconst c/org-dir "~/org/"
  "Path to org files.")

(defconst c/org-roam-dir (concat c/org-dir "personal/roam/")
  "Path to org-roam files.")

(setq user-mail-address "andreas@arvidsson.io")

(defgroup chip ()
  "Options for my personal configuration."
  :group 'config
  :prefix "chip-"
  :tag "Chip")

;;; Straight

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

;;; Deferred compilation

(setq comp-deferred-compilation nil)
(setq native-comp-async-report-warnings-errors nil)

(defun chip/native-compile ()
  (interactive)
  (native-compile-async user-emacs-directory 'recursively))

;;; Load path

(add-to-list 'load-path chip-config-src-dir)
(add-to-list 'load-path (concat user-emacs-directory "scripts"))

;;; Dependencies

(use-package dash)
(use-package s)

;;; Autoloads

(defun chip/update-all-autoloads ()
  "Update autoloaded definitions in loaddefs.el."
  (interactive)
  (cd chip-config-src-dir)
  (let ((generated-autoload-file (expand-file-name "loaddefs.el")))
    ;; create empty file if not exists
    (when (not (file-exists-p generated-autoload-file))
      (with-current-buffer (find-file-noselect
                            generated-autoload-file)
        (insert ";;")
        (save-buffer)))
    (mapc #'update-directory-autoloads '(""))))

(load (concat chip-config-src-dir "loaddefs.el") nil t)

;;; Core functionality

(use-package general)

;; Needs to be loaded before other org code when using straight
(straight-use-package 'org-contrib)

(require 'chip-macro)

;;; Emacs behavior

;; Toggle between cmd/option as meta key (useful for macbook keyboard)
(when (and (eq system-type 'darwin) (display-graphic-p))
  (defun c/toggle-meta ()
    (interactive)
    (if (eq mac-option-modifier 'meta)
        (progn
          (setq mac-option-modifier 'super
                mac-command-modifier 'meta)
          (message "Using cmd as meta"))
      (setq mac-option-modifier 'meta
            mac-command-modifier 'super)
      (message "Using option as meta"))))

;; break at line 80 by default when using functions like `fill-paragraph`
(setq-default fill-column 80)

;; show trailing whitespaces
(add-hook 'prog-mode-hook (lambda ()
                            (setq show-trailing-whitespace t)))

;; Show message when garbage collection happens
(setq-default garbage-collection-messages t)

;;; Platform initialization

;; Use a large GC threshold for initialization
(setf gc-cons-threshold 1073741824)

;; Language servers often generate large responses
(setq read-process-output-max (* 10 1024 1024))

;; Fix for dired not working on Mac OS (requires `brew install coreutils`)
(when c/mac?
  (setq insert-directory-program "/opt/homebrew/bin/gls"))

(require 'chip-init-desktop)
;; (require 'chip-init-android)

;;; Garbage collection

;; Use GCMH to trigger GC when idle
(use-package gcmh
  :config
  (c/diminish gcmh-mode)
  (gcmh-mode 1)
  (setf gcmh-idle-delay 5))

;;; Emacs server

(require 'server)
(unless (server-running-p)
  (server-start))

;;; init.el ends here
