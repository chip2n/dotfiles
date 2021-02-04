;;; chip-doc.el -*- lexical-binding: t -*-

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
(require 'general)

(defvar c/doc-directory "/home/chip/ebooks")

(general-define-key
 "C-c d" 'c/doc-view)

(defun c/--doc-open (path)
  "Open document with Zathura."
  (call-process-shell-command (format "zathura %s &" path) nil 0))

(defun c/doc-view ()
  "View documents from directories listed in `c/doc-directories`."
  (interactive)
  (let ((files (--map (let ((name (file-name-nondirectory it)))
                        (cons name it))
                      (directory-files-recursively c/doc-directory ".+.pdf"))))
    (--> (completing-read "doc: " files)
      (cdr (assoc it files))
      (c/--doc-open it))))

(provide 'chip-doc)

;;; chip-doc.el ends here
