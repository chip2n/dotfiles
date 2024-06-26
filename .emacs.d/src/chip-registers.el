;;; chip-registers.el -*- lexical-binding: t -*-

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

;; This file contains code for working with registers.

;;; Code:

(defun c/jump-to-register ()
  (interactive)
  ;; We always want to match the actual register letter first - if more letters
  ;; are used in the minibuffer input, we switch over to orderless matching
  (let ((completion-styles '(basic orderless)))
    (consult-register))
  (recenter-top-bottom))

(general-define-key
 "C-c r @" 'point-to-register
 "C-c r +" 'increment-register
 "C-c r p" 'insert-register
 "C-c r n" 'number-to-register
 "C-c r y" 'copy-to-register
 "C-c r j" 'c/jump-to-register
 ;; "C-c R" 'consult-register-store
 )

(provide 'chip-registers)

;;; chip-registers.el ends here
