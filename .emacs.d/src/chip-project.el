;;; chip-project.el -*- lexical-binding: t -*-

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

;; This file contains code for working with projects.

;;; Code:

(defmacro with-project-root (&rest body)
  "Run `body' in the root of the project as located by projectile."
  (let ((project-root-buffer (gensym)))
    `(save-excursion
       (let ((,project-root-buffer (find-file-noselect (projectile-project-root))))
         (set-buffer ,project-root-buffer)
         ,@body
         (kill-buffer ,project-root-buffer)))))

(provide 'chip-project)

;;; chip-project.el ends here