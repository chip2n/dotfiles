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
   "C-c w 2"   'split-window-vertically
   "C-c w 3"   'split-window-horizontally
   "C-c w u"   'winner-undo
   "C-c w r"   'winner-redo
   "C-c w n"   'windmove-down
   "C-c w e"   'windmove-up
   "C-c w h"   'windmove-left
   "C-c w i"   'windmove-right
   "C-c w o"   'ace-window
   "C-c w w"   'c/set-window-width
   "C-c w z"   'zoom-mode
   "C-c w f"   'delete-other-windows
   "C-c w k"   'delete-window
   "C-x +"     'chip/window-zoom
   "C-x -"     'chip/window-unzoom
   "C-x ="     'balance-windows
   "M-o"       'ace-window
   "C-<tab>"   'c/next-window
   "C-<iso-lefttab>"   'c/prev-window
   "<next>" 'c/scroll-half-page-down
   "<prior>" 'c/scroll-half-page-up
   "C-v" 'c/scroll-half-page-down
   "M-v" 'c/scroll-half-page-up
   "S-<next>"  'c/scroll-half-page-down-other-window
   "S-<prior>" 'c/scroll-half-page-up-other-window
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

(defun c/scroll-half-page-up-other-window ()
  (interactive)
  (other-window 1)
  (c/scroll-half-page-up)
  (other-window -1))

(defun c/scroll-half-page-down-other-window ()
  (interactive)
  (other-window 1)
  (c/scroll-half-page-down)
  (other-window -1))

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
  :after (posframe)
  :config
  (c/diminish ace-window-mode)
  (setq aw-dispatch-always t)
  (ace-window-posframe-mode)
  (setq aw-posframe-position-handler #'posframe-poshandler-window-bottom-left-corner)

  ;; M-o M-o switches to next window
  (add-to-list 'aw-dispatch-alist '(?\O c/next-window))
  (add-to-list 'aw-dispatch-alist '(?\h aw-split-window-horz "Split Horz Window"))
  (setq aw-translate-char-function
        (lambda (c) (if (= c ?\M-o) ?O c)))

  (defun aw-window< (wnd1 wnd2)
    "Redefinind aw-window< to do row-major ordering."
    (let* ((f1 (window-frame wnd1))
           (f2 (window-frame wnd2))
           (e1 (window-edges wnd1))
           (e2 (window-edges wnd2))
           (p1 (frame-position f1))
           (p2 (frame-position f2))
           (nl (or (null (car p1)) (null (car p2)))))
      (cond ((and (not nl) (< (car p1) (car p2)))
             (not aw-reverse-frame-list))
            ((and (not nl) (> (car p1) (car p2)))
             aw-reverse-frame-list)
            ((< (cadr e1) (cadr e2))
             t)
            ((> (cadr e1) (cadr e2))
             nil)
            ((< (car e1) (car e2))
             t))))

  (defun aw--lead-overlay-posframe (path leaf)
    "Redefining aw--lead-overlay-posframe to display the labels slightly differently."
    (let* ((wnd (cdr leaf))
           (str (format " %s " (apply #'string path)))
           ;; It's important that buffer names are not unique across
           ;; multiple invocations: posframe becomes very slow when
           ;; creating new frames, and so being able to reuse old ones
           ;; makes a huge difference. What defines "able to reuse" is
           ;; something like: a frame exists which hasn't been deleted
           ;; (with posframe-delete) and has the same configuration as
           ;; the requested new frame.
           (bufname (format " *aw-posframe-buffer-%s*" path)))
      (with-selected-window wnd
        (push bufname aw--posframe-frames)
        (posframe-show bufname
                       :string str
                       :poshandler aw-posframe-position-handler
                       :font (face-font 'aw-leading-char-face)
                       :foreground-color (face-foreground 'aw-leading-char-face nil t)
                       :background-color (face-background 'aw-leading-char-face nil t))))))

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

;; (setq split-height-threshold 120
;;       split-width-threshold 160)

;; (defun my-split-window-sensibly (&optional window)
;;     "replacement `split-window-sensibly' function which prefers vertical splits"
;;     (interactive)
;;     (let ((window (or window (selected-window))))
;;         (or (and (window-splittable-p window t)
;;                  (with-selected-window window
;;                      (split-window-right)))
;;             (and (window-splittable-p window)
;;                  (with-selected-window window
;;                      (split-window-below))))))

;; (setq split-window-preferred-function #'my-split-window-sensibly)

(provide 'chip-window)

;;; chip-window.el ends here
