;;; chip-window.el -*- lexical-binding: t -*-

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

;;; Commentary:

;; This file contains code for managing windows.

;;; Code:

;;;; Windows

;; When splitting windows, I often also want to switch to the new buffer

(after-load (general)
  (general-define-key
   "C-x 2"     'split-window-vertically
   "C-x 3"     'split-window-horizontally
   "C-c w u"   'winner-undo
   "C-c w r"   'winner-redo
   "C-c w n"   'c/set-window-width
   "C-x +"     'chip/window-zoom
   "C-x -"     'chip/window-unzoom
   "C-x ="     'balance-windows
   "M-o"       'ace-window
   "C-<tab>"   'c/next-window
   "C-<iso-lefttab>"   'c/prev-window
   "<next>" 'c/scroll-half-page-down
   "<prior>" 'c/scroll-half-page-up
   "S-<next>"  'scroll-other-window
   "S-<prior>" 'scroll-other-window-down
   "S-<up>" 'c/prev-line-center
   "S-<down>" 'c/next-line-center))

(setq scroll-margin 3)

(defun c/window--top-line-number ()
  (save-excursion
    (move-to-window-line 0)
    (1- (line-number-at-pos (point)))))

(defun c/window--bottom-line-number ()
  (save-excursion
    (move-to-window-line (1- (truncate (window-screen-lines))))
    (line-number-at-pos (point))))

(defun c/window--get-line-number ()
  (cdr (posn-actual-col-row (posn-at-point))))

(defun c/scroll-half-page-up ()
  "Scrolls exactly half page up keeping cursor/point position."
  (interactive)
  (let* ((line-win-pos (c/window--get-line-number))
         (line-delta (/ (truncate (window-screen-lines)) 2))
         (window-first-line (c/window--top-line-number)))
    (if (<= (- window-first-line line-delta) 0)
        (progn
          (beginning-of-buffer)
          (recenter-top-bottom 0)
          (when (> (- line-win-pos line-delta) 0)
            (forward-line (- line-win-pos line-delta))))
      (progn
        (scroll-down line-delta)
        (move-to-window-line line-win-pos)))))

(defun c/scroll-half-page-down ()
  "Scrolls exactly half page down keeping cursor/point position."
  (interactive)
  (let* ((line-win-pos (c/window--get-line-number))
         (line-delta (/ (truncate (window-screen-lines)) 2))
         (window-bottom-line (c/window--bottom-line-number))
         (last-buffer-line (1- (line-number-at-pos (point-max))))
         (curr-line (1- (line-number-at-pos (point)))))
    (if (>= (+ window-bottom-line line-delta) last-buffer-line)
        (progn
          (end-of-buffer)
          (recenter-top-bottom -1)
          (when (< (+ curr-line line-delta) last-buffer-line)
            (forward-line (- (- last-buffer-line (+ curr-line line-delta))))))
        (progn
          (scroll-up line-delta)
          (move-to-window-line line-win-pos)))))

(defun c/next-window ()
  (interactive)
  (other-window 1))

(defun c/prev-window ()
  (interactive)
  (other-window -1))

(defun c/prev-line-center ()
  (interactive)
  (evil-previous-line)
  (scroll-down 1))

(defun c/next-line-center ()
  (interactive)
  (evil-next-line)
  (scroll-up 1))

(defun c/set-window-width (n)
  "Set the selected window's width."
  (interactive (list (read-number "Width: " 80)))
  (adjust-window-trailing-edge (selected-window)
                               (- (+ n (line-number-display-width) 3)
                                  (window-width)) t))

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

(defun c/avy-action-embark (pt)
  (unwind-protect
      (save-excursion
        (goto-char pt)
        (embark-act))
    (select-window
     (cdr (ring-ref avy-ring 0))))
  t)

(setf (alist-get ?, avy-dispatch-alist) 'c/avy-action-embark)

(defun c/avy-action-kill-lines (pt)
  (save-excursion
    (beginning-of-line)
    (let ((start (point)))
      (goto-char pt)
      (end-of-line)
      (kill-region start (+ 1 (point))))))

(setf (alist-get ?l avy-dispatch-alist) 'c/avy-action-kill-lines)

(use-package ace-window
  :config
  (c/diminish ace-window-mode)
  (setq aw-dispatch-always t)

  ;; M-o M-o switches to next window
  (add-to-list 'aw-dispatch-alist '(?\O c/next-window))
  (setq aw-translate-char-function
        (lambda (c) (if (= c ?\M-o) ?O c))))

(use-package popper
  :bind (("C-<return>" . popper-toggle-latest)
         ("C-S-<return>" . popper-cycle))
  :config
  (setf popper-mode-line nil))

(defun c/split-window-sensibly (&optional window)
  "Attempt to sensibly split the window.
WINDOW defaults to current window. If two non-dedicated windows already exist,
do not split. Otherwise, split along the longest edge."
  (let ((windows (-filter (-compose 'not 'window-dedicated-p)
                          (window-list)))
        (window (or window (selected-window))))
    (when (< (length windows) 2)
      (with-selected-window window
        (if (> (frame-inner-height) (frame-inner-width))
            (split-window-below)
          (split-window-right))))))

(setq split-window-preferred-function 'c/split-window-sensibly)

(defun count-visible-buffers (&optional frame)
  "Count how many buffers are currently being shown. Defaults to selected frame."
  (length (mapcar #'window-buffer (window-list frame))))

(defun do-not-split-more-than-two-windows (window &optional horizontal)
  (if (and horizontal (> (count-visible-buffers) 1))
      nil
    t))

(advice-add 'window-splittable-p :before-while #'do-not-split-more-than-two-windows)

(provide 'chip-window)

;;; chip-window.el ends here
