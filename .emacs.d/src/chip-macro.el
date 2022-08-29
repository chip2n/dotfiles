;;; chip-macro.el -*- lexical-binding: t -*-

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

;;; Code:

(cl-defmacro after-load (pkgs &body body)
  "Waits until all packages are loaded before evaluating body.

Example:

(after-load (ivy counsel projectile)
  (do-stuff))

Expands to:

(with-eval-after-load \"ivy\"
  (with-eval-after-load \"counsel\"
    (with-eval-after-load \"projectile\"
      ...)))"
  (declare (indent 1))
  (if pkgs
      `(with-eval-after-load ,(symbol-name (car pkgs))
         (after-load ,(cdr pkgs) ,@body))
    `(progn ,@body)))

(provide 'chip-macro)

;;; chip-macro.el ends here
