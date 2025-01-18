;;; chip-code-ruby.el -*- lexical-binding: t -*-

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

(use-package robe
  :config
  :hook ((ruby-mode . robe-mode)
         (ruby-ts-mode . robe-mode)))

(defun c/rails-dev ()
  (interactive)
  (with-dominating-file-dir "Gemfile"
    (let* ((program "bin/dev")
           (buffer (get-buffer-create "*rails-server*"))
           (proc-alive (comint-check-proc buffer))
           (process (get-buffer-process buffer)))
      ;; If the process is dead then re-create the process and reset the mode.
      (unless proc-alive
        (with-current-buffer buffer
          (apply 'make-comint-in-buffer "Rails" buffer
                 program nil nil)))
      ;; Regardless, provided we have a valid buffer, we pop to it.
      (when buffer
        (pop-to-buffer buffer)))))

(provide 'chip-code-ruby)

;;; chip-code-ruby.el ends here
