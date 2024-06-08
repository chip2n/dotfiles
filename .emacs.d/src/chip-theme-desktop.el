;;; chip-theme-desktop.el -*- lexical-binding: t -*-

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

(require 'chip-theme-base)

;; Show line numbers where appropriate
(add-hook 'prog-mode-hook #'display-line-numbers-mode)

;; Use minimum of 3 characters to avoid frequent resizing during window height change (e.g. during M-x)
(setq-default display-line-numbers-width 3)

;;; Tab bar

(tab-bar-mode +1)
(setq tab-bar-close-button-show nil)
(setq tab-bar-format '(c/tab-bar
                       ;; tab-bar-format-tabs tab-bar-separator
                       tab-bar-format-align-right
                       ;; tab-bar-format-global
                       ))
(setq tab-bar-auto-width-max '(500 50))

(defun c/tab-bar ()
  (concat
   " "
   (propertize (all-the-icons-fileicon "emacs")
               'face `(:family ,(all-the-icons-fileicon-family) :height 0.8)
               'display '(raise -0.0))
   " "
   (system-name)
   " | "
   (buffer-name)))

;;; Dim unselected windows

(defface c/dimmed-face
  '((t :background "#1b1e24"))
  ""
  )

(defface c/dimmed-face-header-line
  '((t (:background "#21242b"
        :foreground "#6b7385")))
  "")
;; (defface c/dimmed-face-header-line
;;   '((t (:box (:line-width 4 :color "#21242b" :style nil)
;;         :background "#21242b"
;;         :foreground "#6b7385")))
;;   "")

(use-package auto-dim-other-buffers
  :config
  (setf auto-dim-other-buffers-affected-faces
        '(;; (default . c/dimmed-face)
          ;; (fringe . c/dimmed-face)
          (header-line . c/dimmed-face-header-line)
          (line-number-current-line . line-number))
        )

  (auto-dim-other-buffers-mode 1))

;;; Package: all-the-icons

;; Using fancy icons in some places (e.g. treemacs) to spice things up. This
;; package includes icons from a bunch of different sources.

(use-package all-the-icons)

(provide 'chip-theme-desktop)

;;; chip-theme-desktop.el ends here
