;;; chip-meow.el -*- lexical-binding: t -*-

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

;; (defun c/meow-append ()
;;   "Move to the end of selection, switch to INSERT state."
;;   (interactive)
;;   (if meow--temp-normal
;;       (progn
;;         (message "Quit temporary normal mode")
;;         (meow--switch-state 'motion))
;;     (if (not (region-active-p))
;;         (when (and (not (use-region-p))
;;                    (< (point) (point-max)))
;;           (forward-char 1))
;;       (meow--direction-forward)
;;       (meow--cancel-selection))
;;     (meow--switch-state 'insert)))

(defun c/meow-change-save ()
  "Like `meow-change-save`, but calles `meow-change-char' if no region is active."
  (interactive)
  (if (region-active-p)
      (call-interactively #'meow-change-save)
    (call-interactively #'meow-change-char)))

(defun c/meow-find-reverse (ch)
  (interactive "cFind:")
  (meow-find -1 ch))

(defun c/meow-till-reverse (ch)
  (interactive "cTill:")
  (meow-till -1 ch))

(defun c/meow-append ()
  "Move to the end of selection, switch to INSERT state."
  (interactive)
  (if meow--temp-normal
      (progn
        (message "Quit temporary normal mode")
        (meow--switch-state 'motion))
    (if (not (region-active-p))
        (when (and (not (use-region-p))
                   (< (point) (point-max))
                   (not (eolp)))
          (forward-char 1))
      (meow--direction-forward)
      (meow--cancel-selection))
    (meow--switch-state 'insert)))

(defun c/meow-extend-to-end-of-thing (thing)
  "Extend selection to the end of THING."
  (interactive (list (meow-thing-prompt "Extend to end of: ")))
  (if (not (use-region-p))
      (meow-end-of-thing thing)
    (save-window-excursion
      (let ((back (equal 'backward (meow--thing-get-direction 'end)))
            (bounds (meow--parse-inner-of-thing-char thing)))
        (let ((beg (min (point) (mark))))
          (when bounds
            (thread-first
              (meow--make-selection '(select . transient)
                                    (if back (cdr bounds) beg)
                                    (if back beg (cdr bounds)))
              (meow--select))))))))

(defun c/meow-extend-to-beginning-of-thing (thing)
  "Extend selection to the beginning of THING."
  (interactive (list (meow-thing-prompt "Extend to beginning of: ")))
  (if (not (use-region-p))
      (meow-beginning-of-thing thing)
    (save-window-excursion
      (let ((back (equal 'backward (meow--thing-get-direction 'beginning)))
            (bounds (meow--parse-inner-of-thing-char thing)))
        (let ((end (max (point) (mark))))
          (when bounds
            (thread-first
              (meow--make-selection '(select . transient)
                                    (if back end (car bounds))
                                    (if back (car bounds) end))
              (meow--select))))))))

(defun meow-setup ()
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-colemak)
  (setq meow-use-clipboard t)
  ;; Allow SPC <char> to use commands defined in keymaps other than mode-specific-map
  (setq meow-keypad-leader-dispatch "C-c")
  (meow-motion-overwrite-define-key
   ;; Use e to move up, n to move down.
   ;; Since special modes usually use n to move down, we only overwrite e here.
   '("e" . meow-prev)
   '("<escape>" . ignore))
  (meow-leader-define-key
   '("?" . meow-cheatsheet)
   ;; To execute the originally e in MOTION state, use SPC e.
   ;; '("e" . "H-e")
   '("1" . meow-digit-argument)
   '("2" . meow-digit-argument)
   '("3" . meow-digit-argument)
   '("4" . meow-digit-argument)
   '("5" . meow-digit-argument)
   '("6" . meow-digit-argument)
   '("7" . meow-digit-argument)
   '("8" . meow-digit-argument)
   '("9" . meow-digit-argument)
   '("0" . meow-digit-argument)
   ;; '("SPC" . execute-extended-command)
   '("TAB" . other-window)
   '("u" . meow-universal-argument))
  (meow-normal-define-key
   '("0" . meow-expand-0)
   '("1" . meow-expand-1)
   '("2" . meow-expand-2)
   '("3" . meow-expand-3)
   '("4" . meow-expand-4)
   '("5" . meow-expand-5)
   '("6" . meow-expand-6)
   '("7" . meow-expand-7)
   '("8" . meow-expand-8)
   '("9" . meow-expand-9)
   '("-" . negative-argument)
   '(";" . meow-reverse)
   '("," . meow-inner-of-thing)
   '("." . meow-bounds-of-thing)
   '("[" . meow-beginning-of-thing)
   '("]" . meow-end-of-thing)
   '("{" . c/meow-extend-to-beginning-of-thing)
   '("}" . c/meow-extend-to-end-of-thing)
   '("/" . meow-visit)
   '("a" . c/meow-append)
   '("A" . meow-open-below)
   '("b" . meow-back-word)
   '("B" . meow-back-symbol)
   '("c" . c/meow-change-save)
   '("d" . meow-delete)
   '("e" . meow-prev)
   '("E" . meow-prev-expand)
   '("f" . meow-find)
   '("F" . c/meow-find-reverse)
   '("g" . meow-cancel-selection)
   '("G" . meow-grab)
   '("h" . meow-left)
   '("H" . meow-left-expand)
   '("i" . meow-right)
   '("I" . meow-right-expand)
   '("j" . meow-join)
   '("k" . meow-kill)
   '("l" . meow-line)
   '("L" . meow-goto-line)
   '("m" . meow-mark-word)
   '("M" . meow-mark-symbol)
   '("n" . meow-next)
   '("N" . meow-next-expand)
   '("o" . meow-block)
   '("O" . meow-to-block)
   '("p" . meow-yank)
   '("P" . meow-swap-grab)
   '("q" . meow-quit)
   '("r" . meow-replace)
   '("s" . meow-insert)
   '("S" . meow-open-above)
   '("t" . meow-till)
   '("T" . c/meow-till-reverse)
   '("u" . meow-undo)
   '("U" . meow-undo-in-selection)
   '("v" . meow-search)
   '("w" . meow-next-word)
   '("W" . meow-next-symbol)
   '("x" . meow-delete)
   '("X" . meow-backward-delete)
   '("y" . meow-save)
   '("z" . meow-pop-selection)
   '("<" . meow-pop-to-mark)
   '(">" . meow-unpop-to-mark)
   '("'" . repeat)
   '("<escape>" . ignore)))

(use-package meow
  :config
  (meow-setup)
  (meow-global-mode 1))

(use-package embrace
  :straight (:type git :host github :repo "cute-jumper/embrace.el")
  :bind (("C-c ," . embrace-commander))
  :config
  (add-hook 'org-mode-hook 'embrace-org-mode-hook)
  (defun embrace-markdown-mode-hook ()
    (dolist (lst '((?* "*" . "*")
                   (?\ "\\" . "\\")
                   (?$ "$" . "$")
                   (?/ "/" . "/")))
      (embrace-add-pair (car lst) (cadr lst) (cddr lst))))
  (add-hook 'markdown-mode-hook 'embrace-markdown-mode-hook))

(provide 'chip-meow)

;;; chip-meow.el ends here
