;;; chip-code-dbg.el -*- lexical-binding: t -*-

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

(require 's)

(use-package realgud
  :defer t
  :config
  (setq realgud-safe-mode nil))

(defun c/gdb (path)
  (interactive "FPath to executable: ")
  (let ((source-buf (current-buffer)))
    (delete-other-windows)
    (realgud:gdb (format "gdb %s" path))
    (realgud:attach-source-buffer source-buf)
    (let ((gdb-buf (current-buffer)))
      (split-window-horizontally)
      (switch-to-buffer source-buf)
      (other-window 1)
      (switch-to-buffer gdb-buf))))

(defun c/gdb-attach ()
  (interactive)
  (let* ((output (s-lines (shell-command-to-string "ps -u chip -o pid,comm")))
         (lines (-filter (-compose #'not #'s-blank?) (cdr output)))
         (processes (mapcar (lambda (line)
                              (let* ((split (s-split " " (s-trim line)))
                                     (pid (string-to-number (s-trim (car split))))
                                     (comm (s-trim (cadr split))))
                                (cons comm pid)))
                            lines))
         (pid (-> (completing-read "Attach to process:" processes)
                (assoc processes)
                (cdr)))
         (source-buf (current-buffer)))
        (save-window-excursion
          (realgud:gdb-pid pid)
          (realgud:attach-source-buffer source-buf))
        (delete-other-windows)
        (split-window-horizontally)
        (other-window 1)
        (switch-to-buffer (realgud:gdb-find-command-buffer pid))))

(provide 'chip-code-dbg)

;;; chip-code-dbg.el ends here
