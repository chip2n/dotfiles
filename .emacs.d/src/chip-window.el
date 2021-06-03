;;; chip-window.el -*- lexical-binding: t -*-

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

;;; Commentary:

;; This file contains code for managing windows.

;;; Code:
;; When splitting windows, I often also want to switch to the new buffer

(after-load (general)
  (general-define-key
   "C-x 2"     (lambda () (interactive) (split-window-vertically) (other-window 1))
   "C-x 3"     (lambda () (interactive) (split-window-horizontally) (other-window 1))
   "C-c w u"   'winner-undo
   "C-c w r"   'winner-redo
   "C-x +"     'chip/window-zoom
   "C-x -"     'chip/window-unzoom
   "C-x ="     'balance-windows
   "M-o"       'ace-window
   "S-<next>"  'scroll-other-window
   "S-<prior>" 'scroll-other-window-down))

(global-set-key [C-tab] 'evil-window-next)
(global-set-key [C-iso-lefttab] 'evil-window-prev)

;; Save window configurations as bookmarks
(use-package burly
  :bind (("C-c w s" . burly-bookmark-windows))
  :config
  (setq burly-bookmark-prefix "win: "))

;; Automatic window zooming
(use-package zoom
  :config
  (setq zoom-size '(0.618 . 0.618))
  (c/diminish zoom-mode))

(defun chip/window-zoom ()
  (interactive)
  (zoom))

(defun chip/window-unzoom ()
  (interactive)
  (other-window 1)
  (unwind-protect
      (chip/window-zoom)
    (other-window 1)))

;; Window layout undo/redo
(use-package winner
  :config
  (winner-mode 1))

;; Allows you to transpose frames (mainly via ace-window)
(require 'transpose-frame)

(use-package avy
  :config
  (setq avy-timeout-seconds 0.3)
  ;; colemak homerow
  (setq avy-keys '(?a ?r ?s ?t ?d ?h ?n ?e ?i ?o)))

(use-package ace-window
  :after (ivy)
  :config
  (c/diminish ace-window-mode)
  (setq aw-dispatch-always t)
  (after-load (ivy counsel projectile)
    (ivy-add-actions
     'ivy-switch-buffer
     '(("a" ace-window "ace-window")))
    (ivy-add-actions
     'counsel-find-file
     '(("a" ace-window "ace-window")))
    (ivy-add-actions
     'counsel-projectile-find-file
     '(("a" ace-window "ace-window")))))

(use-package popper
  :bind (("C-<return>" . popper-toggle-latest)
         ("C-S-<return>" . popper-cycle))
  :config
  (setf popper-mode-line nil))

(provide 'chip-window)

;;; chip-window.el ends here
