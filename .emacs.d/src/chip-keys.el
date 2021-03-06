;;; chip-keys.el -*- lexical-binding: t -*-

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

;; This file contains "global" keybindings that are accessible in most
;; contexts. Package-specific keybindings are generally set up in the
;; same place as the other package configuration.

;;; Code:

(defun chip/find-file ()
  (interactive)
  (call-interactively #'find-file))

(defun chip/find-file-project-current ()
  (interactive)
  (projectile-find-file))

(defun chip/find-file-project ()
  (interactive)
  (projectile-switch-project))

(defun chip/switch-buffer ()
  (interactive)
  (consult-buffer))

(defun chip/switch-buffer-other-window ()
  (interactive)
  (consult-buffer-other-window))

(defun chip/grep ()
  (interactive)
  (consult-ripgrep))

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

(general-define-key
 :states '(emacs normal insert visual motion)

 ;; file system
 "C-c c" 'chip/open-config-file
 "C-f" 'chip/find-file
 "C-p" 'chip/find-file-project-current
 "C-x p" 'chip/find-file-project-current
 "C-S-P" 'chip/find-file-project
 "C-x P" 'chip/find-file-project

 ;; buffers
 "C-b" 'chip/switch-buffer
 "C-x b" 'chip/switch-buffer
 "C-S-B" 'chip/switch-buffer-other-window

 ;; searching
 "C-c g" 'chip/grep

 ;; org-mode agenda
 "C-c a" 'chip/org-agenda

 ;; org-mode capture
 "C-c e" 'chip/capture

 ;; org-mode clocking
 "C-c o i" 'chip/clock-in-task
 "C-c o o" 'chip/clock-out-task
 "C-c o g" 'chip/jump-to-clocked-task

 ;; quickly jump to an org headline
 "C-c j" 'c/org-jump-to-headline

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

(provide 'chip-keys)

;;; chip-keys.el ends here
