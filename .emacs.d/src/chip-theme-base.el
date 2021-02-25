;;; chip-theme-base.el -*- lexical-binding: t -*-

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

(defgroup chip-theme nil
  "Options for my personal theme")
(load-theme 'chip t)

;; Disable all the GUI fluff
(when (display-graphic-p)
    (scroll-bar-mode -1))
(menu-bar-mode -1)
(tool-bar-mode -1)

;; Display underline further away from the text
(setq x-underline-at-descent-line t)

;; Hide welcome screen
(setq inhibit-startup-screen t)

;; Highlight the current line
(global-hl-line-mode +1)

;; Inhibit startup message
(setq inhibit-startup-echo-area-message "chip")

(provide 'chip-theme-base)



(provide 'chip-theme-base)

;;; chip-theme-base.el ends here
