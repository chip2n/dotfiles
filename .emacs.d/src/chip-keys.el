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

;;; Code:

(general-define-key
     :prefix "C-c"
     ;; org-mode agenda
     "a" 'chip/org-agenda

     ;; org-mode capture
     "e" 'org-capture

     ;; org-mode clocking
     "o i" 'org-clock-in
     "o o" 'org-clock-out
     "o g" 'org-clock-goto

     ;; quickly jump to a task in your agenda files
     "j" 'counsel-org-agenda-headlines)

(provide 'chip-keys)

;;; chip-keys.el ends here
