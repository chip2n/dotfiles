;;; chip-keys.el -*- lexical-binding: t -*-

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

;; This file contains "global" keybindings that are accessible in most
;; contexts. Package-specific keybindings are generally set up in the
;; same place as the other package configuration.

;;; Code:

(defun chip/find-file ()
  (interactive)
  (call-interactively #'find-file))

(defun chip/switch-buffer ()
  (interactive)
  (consult-buffer))

(defun chip/switch-buffer-other-window ()
  (interactive)
  (consult-buffer-other-window))

(defun c/grep (prefix)
  "Grep in current projectile directory.
If prefix is used, grep in current directory instead."
  (interactive "P")
  (consult-ripgrep (if (not prefix) nil t)))

(defun chip/capture ()
  (interactive)
  (org-capture))

(defun chip/jump-to-clocked-task ()
  (interactive)
  (org-clock-goto))

(defun chip/clock-in-task ()
  (interactive)
  (org-clock-in))

(defun chip/clock-out-task ()
  (interactive)
  (org-clock-out))

(defun chip/calendar ()
  (interactive)
  (org-goto-calendar))

(defun chip/history-back ()
  (interactive)
  (pop-global-mark)
  ;(evil-jump-backward)
  )

(defun chip/history-forward ()
  (interactive)
  (evil-jump-forward))

(general-define-key
 ;; file system
 ;; "C-f" 'chip/find-file
 ;; "C-p" 'projectile-find-file
 "C-x p" 'projectile-find-file
 "C-S-P" 'projectile-switch-project
 "C-x P" 'projectile-switch-project

 ;; buffers
 "C-x b" 'chip/switch-buffer
 "C-x C-b" 'ibuffer

 ;; navigation
 "C-o" 'chip/history-back
 "C-S-o" 'chip/history-forward

 ;; searching
 "C-c g" 'c/grep

 ;; org-mode agenda
 "C-c a" 'c/org-agenda

 ;; org-mode capture
 "C-c e" 'chip/capture

 ;; org-mode clocking
 "C-c o i" 'chip/clock-in-task
 "C-c o o" 'chip/clock-out-task
 "C-c o g" 'chip/jump-to-clocked-task

 ;; quickly jump to an org headline
 "C-c j" 'c/org-jump-to-headline
 "C-c J" 'c/project-org-jump

 ;; code
 "M-." 'xref-find-definitions
 "M-," 'xref-pop-marker-stack
 "C-h ." 'describe-thing-at-point)

;; org-mode agenda buffer
(general-define-key
 :keymaps 'org-agenda-mode-map
 "j" 'c/org-agenda-jump-to-task
 "RET" 'org-agenda-switch-to
 "C-g" 'org-agenda-quit)

(general-define-key
 :states '(normal)
 "+" 'text-scale-increase
 "-" 'text-scale-decrease)

;;; Hydra

(use-package hydra)
(use-package major-mode-hydra)

(provide 'chip-keys)

;;; chip-keys.el ends here
