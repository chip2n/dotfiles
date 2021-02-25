;;; chip-colors-base.el -*- lexical-binding: t -*-

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

(defgroup chip '()
  "Faces and colors for the chip emacs theme")

(defvar chip-base-colors--defaults
  `((foreground . ,(face-foreground 'default nil t))
    (background . ,(face-background 'default nil t))
    (discrete   . ,(face-background 'mode-line-inactive nil t))
    (highlight  . ,(face-background 'fringe nil t))

    (critical   . ,(face-foreground 'error nil t))
    (salient    . ,(face-foreground 'font-lock-keyword-face nil t))
    (strong     . ,(face-foreground 'default nil t))
    (popout     . ,(face-foreground 'font-lock-string-face nil t))
    (faded      . ,(face-foreground 'shadow nil t))))

(defun chip-base-colors--get (name)
  "Get default color associated with symbol NAME."
  (cdr (assoc name chip-base-colors--defaults)))

(defcustom chip-color-foreground (chip-base-colors--get 'foreground)
  ""
  :type 'color
  :group 'chip)

(defcustom chip-color-background (chip-base-colors--get 'background)
  ""
  :type 'color
  :group 'chip)

(defcustom chip-color-discrete (chip-base-colors--get 'discrete)
  ""
  :type 'color
  :group 'chip)

(defcustom chip-color-highlight (chip-base-colors--get 'highlight)
  ""
  :type 'color
  :group 'chip)

(defcustom chip-color-blue (chip-base-colors--get 'faded)
  ""
  :type 'color
  :group 'chip)

(defcustom chip-color-pink (chip-base-colors--get 'faded)
  ""
  :type 'color
  :group 'chip)

(provide 'chip-colors-base)

;;; chip-colors-base.el ends here
