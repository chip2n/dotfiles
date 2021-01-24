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

;;; Directories

(defconst chip-config-dir "~/.emacs.d/"
  "Path to emacs config directory.")

(defconst chip-config-src-dir (concat chip-config-dir "src/")
  "Path to emacs config src directory.")

;;; Load path

(add-to-list 'load-path "~/.emacs.d/new")
(add-to-list 'load-path chip-config-src-dir)
(add-to-list 'load-path "~/.emacs.d/scripts")

;;; Deferred compilation

(setq comp-deferred-compilation nil)

(defun chip/native-compile ()
  (interactive)
  (native-compile-async "~/.emacs.d" 'recursively))

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

;;; Modeline

;; taken from:
;; https://www.masteringemacs.org/article/hiding-replacing-modeline-strings
(defvar mode-line-cleaner-alist
  `((lisp-interaction-mode . "lisp-interaction")
    (emacs-lisp-mode . "elisp")
    (magit-status-mode . "magit")
    (org-mode . "org")
    (messages-buffer-mode . "messages"))
  "Alist for `clean-mode-line'.

When you add a new element to the alist, keep in mind that you
must pass the correct minor/major mode symbol and a string you
want to use in the modeline *in lieu of* the original.")

(defun clean-mode-line ()
  (interactive)
  (cl-loop for cleaner in mode-line-cleaner-alist
        do (let* ((mode (car cleaner))
                  (mode-str (cdr cleaner))
                  (old-mode-str (cdr (assq mode minor-mode-alist))))
             (when old-mode-str
               (setcar old-mode-str mode-str))
             ;; major mode
             (when (eq mode major-mode)
               (setq mode-name mode-str)))))

(add-hook 'after-change-major-mode-hook 'clean-mode-line)

(use-package telephone-line
  :after (evil)
  :config
  (setq telephone-line-lhs
        '((evil   . (telephone-line-evil-tag-segment))
          (accent . (telephone-line-vc-segment))
          (nil    . (telephone-line-process-segment
                     telephone-line-minor-mode-segment))
	  ))
  (setq telephone-line-rhs
        '((nil    . (telephone-line-erc-modified-channels-segment))
          (nil    . (telephone-line-misc-info-segment))
          (nil . (telephone-line-major-mode-segment))
          (evil   . (telephone-line-airline-position-segment))))
  (telephone-line-mode t))


;; I try to diminish most minor modes to keep the modeline free. Most are removed
;; entirely, and some are shortened.

(use-package diminish
  :after (ivy projectile evil-snipe evil-lispy org-roam company-box)
  :config
  (diminish 'auto-fill-function)
  (diminish 'undo-tree-mode)
  (diminish 'company-mode)
  (diminish 'counsel-mode)
  (diminish 'ivy-mode)
  (diminish 'projectile-mode)
  (diminish 'projectile-mode)
  (diminish 'evil-snipe-mode)
  (diminish 'evil-snipe-local-mode)
  (diminish 'lispy-mode)
  (diminish 'evil-lispy-mode)
  (diminish 'auto-revert-mode "rev")
  (diminish 'emacs-lisp-mode "elisp")
  (diminish 'eldoc-mode)
  (diminish 'yas-minor-mode)
  (diminish 'evil-org-mode)
  (diminish 'org-indent-mode)
  (diminish 'org-roam-mode)
  (diminish 'org-src-mode)
  (diminish 'outshine-mode)
  (diminish 'which-key-mode)
  (diminish 'outline-minor-mode)
  (diminish 'slime-autodoc-mode)
  (diminish 'slime-mode "slime")
  (diminish 'company-box-mode)
  (diminish 'flycheck-mode)
  (diminish 'ace-window-mode))

;;; Desktop initialization

(require 'chip-init-desktop)
;; (require 'chip-init-android)

;;; init.el ends here
