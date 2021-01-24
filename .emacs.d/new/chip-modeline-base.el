;;; chip-modeline-base.el -*- lexical-binding: t -*-

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

(require 'dash)
(require 'cl-lib)

(defvar chip-modeline-modelines-alist nil
  "Determines which modeline should be installed for each major mode")

(defun chip-modeline--modelines-get ()
  "Get the first matching modeline from `chip-modeline-modelines-alist'"
  (cl-dolist (entry chip-modeline-modelines-alist)
    (let ((predicate-sym (car entry))
          (modeline-fn (cdr entry)))
      (unless (or (symbol-function predicate-sym)
                  (eq predicate-sym t))
        (error "Keys in `chip-modeline--modelines-get' must be predicate functions or `t`"))
      (when (or (eq predicate-sym t)
                (funcall (symbol-function predicate-sym)))
        (cl-return (funcall (symbol-function (alist-get predicate-sym chip-modeline-modelines-alist))))))))

(defun chip-modeline-format (left right)
  "Return a string of `window-width' length.
Containing LEFT, and RIGHT aligned respectively."
  (let ((left (-map (lambda (x)
                      (if (stringp x)
                          x
                        (funcall (symbol-function x)))) left))
        (right (-map (lambda (x)
                       (if (stringp x)
                           x
                         (funcall (symbol-function x)))) right)))
    (let ((available-width
           (- (window-total-width)
              (+ (length (format-mode-line left))
                 (length (format-mode-line right)))
              )))
      (append left
              (list (format (format "%%%ds" available-width) ""))
              right))))

(setq-default mode-line-format '((:eval (chip-modeline--modelines-get))))

(defun chip-modeline--update-colors ()
  "Update modeline colors for inactive windows.
Stolen from URL `https://emacs.stackexchange.com/a/3522'."
  (mapc
   (lambda (window)
     (with-current-buffer (window-buffer window)
       (let ((original-format (get 'mode-line-format 'original)))
         ;; if we didn't save original format yet, do it now
         (when (not original-format)
           (put 'mode-line-format 'original mode-line-format)
           (setq original-format mode-line-format))
         ;; check if this window is selected, set faces accordingly
         (if (eq window (selected-window))
             (setq mode-line-format original-format)
           (setq mode-line-format "")))))
   (window-list)))
(add-hook 'window-configuration-change-hook #'chip-modeline--update-colors)

(defun chip-modeline ()
  "Initialize modeline for the current mode"
  (interactive)
  (setq mode-line-format '((:eval (chip-modeline--modelines-get)))))

(provide 'chip-modeline-base)

;;; chip-modeline-base.el ends here
