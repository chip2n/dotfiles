;;; chip-modeline.el -*- lexical-binding: t -*-

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

(require 'chip-modeline-base)

(require 's)

;;; configuration

(setq chip-modeline-modelines-alist
  '(;; (chip-modeline-prog-p . chip-modeline-prog)
    ;; (chip-modeline-doc-p . chip-modeline-doc)
    ;; (chip-modeline-agenda-p . chip-modeline-agenda)
    (t . chip-modeline-fallback)))

;; modeline: fallback

(defun chip-modeline-fallback ()
  "Modeline used as a fallback if no other modeline is available"
  (chip-modeline-format
   '(chip-modeline-tag-evil-state
     " ["
     chip-modeline-tag-major-mode
     "] "
     chip-modeline-tag-vc
     chip-modeline-misc-info
     chip-modeline-process-info
     "%e ")
   '(chip-modeline-minor-modes
     "  ")))

;; tags

(defun chip-modeline-tag-major-mode ()
  "Tag used to display the current major mode"
  (case major-mode
    ('emacs-lisp-mode "elisp")
    ('org-agenda-mode "agenda")
    (t (s-chop-suffix "-mode" (symbol-name major-mode)))))

(defun chip-modeline-tag-vc ()
  "Tag used to display the current VC branch"
  (if vc-mode
      (let ((backend (vc-backend buffer-file-name)))
        (concat "#" (substring-no-properties vc-mode (+ (if (eq backend 'Hg) 2 3) 2))))
    ""))

(defun chip-modeline-tag-evil-state ()
  "Tag used to display which evil state the buffer is in"
  (cond
   ((evil-insert-state-p) (propertize " " 'face 'chip-face-evil-state-insert))
   ((evil-emacs-state-p)  (propertize " " 'face 'chip-face-evil-state-emacs))
   ((evil-visual-state-p) (propertize " " 'face 'chip-face-evil-state-visual))
   (t (propertize " " 'face 'chip-face-evil-state-normal))))

;; (defun chip-modeline-tag-org-clock ()
;;   (concat " | " (all-the-icons-material "timer")
;;           (substring-no-properties org-mode-line-string)))

(defun chip-modeline-misc-info ()
  (let ((content (format-mode-line mode-line-misc-info)))
    (if (s-blank-str? content)
        ""
      (concat " | " content))))

(defun chip-modeline-process-info ()
  mode-line-process)

(defun chip-modeline-minor-modes ()
  minor-mode-alist)

(provide 'chip-modeline)

;;; chip-modeline.el ends here
