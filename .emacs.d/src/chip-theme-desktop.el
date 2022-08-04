;;; chip-theme-desktop.el -*- lexical-binding: t -*-

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

(require 'chip-theme-base)

;; Show line numbers where appropriate
(add-hook 'prog-mode-hook #'display-line-numbers-mode)

;; Use minimum of 3 characters to avoid frequent resizing during window height change (e.g. during M-x)
(setq-default display-line-numbers-width 3)

(provide 'chip-theme-desktop)

;;; chip-theme-desktop.el ends here
