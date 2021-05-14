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

(use-package realgud)

(defun c/gdb ()
  (interactive)
  (save-window-excursion
    (gdb "gdb -i=mi /home/chip/dev/zig-sdl/zig-out/bin/zig-sdl")
    (gdb-display-memory-buffer))
  (delete-other-windows)
  (split-window-below -20)
  (other-window 1)
  (switch-to-buffer gud-comint-buffer)
  (split-window-right)
  (other-window 1)
  (switch-to-buffer (gdb-get-buffer 'gdb-memory-buffer)))

(use-package gdb-mi
  :straight (:type git :host github :protocol ssh :repo "weirdNox/emacs-gdb")
  :general (:keymaps 'gdb-keys-mode-map
            :states 'normal
            "r" 'gdb-run
            "n" 'gdb-next
            "s" 'gdb-step
            "w" 'gdb-watcher-add
            "b" 'gdb-toggle-breakpoint
            "q" 'gdb-kill-session)
  :config
  (fmakunbound 'gdb)
  (fmakunbound 'gdb-enable-debug))

(provide 'chip-code-dbg)

;;; chip-code-dbg.el ends here
