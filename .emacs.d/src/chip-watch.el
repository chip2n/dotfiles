;;; chip-watch.el -*- lexical-binding: t -*-

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

(use-package impatient-mode)

(defun c/watch-file (path)
  (interactive "fWatch file: ")

  (save-window-excursion
    (with-current-buffer (find-file path)
      (httpd-start)
      (impatient-mode)
      (auto-revert-mode 1))))

(defun c/unwatch-file ()
  (interactive)
  (unless c/watched-files
    (error "No files are being watched"))

  (let ((path (completing-read "Unwatch file: " c/watched-files)))
    (save-window-excursion
      (with-current-buffer (find-file path)
        (auto-revert-mode -1)
        (impatient-mode -1)))))

(provide 'chip-watch)

;;; chip-watch.el ends here
