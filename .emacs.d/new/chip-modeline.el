;;; chip-modeline.el -*- lexical-binding: t -*-

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

(require 'cl-lib)
(require 's)
(require 'dash)

(defun chip-modeline-format (left right)
  "Return a string of `window-width' length.
Containing LEFT, and RIGHT aligned respectively."
  (let ((left (-map (lambda (x)
                      (if (stringp x)
                          x
                        (funcall (symbol-function x)))) left))
        (right (-map (lambda (x)
                       (if (stringp x)
                           x
                         (funcall (symbol-function x)))) right)))
    (let ((available-width
           (- (window-total-width)
              (+ (length (format-mode-line left))
                 (length (format-mode-line right)))
              )))
      (append left
              (list (format (format "%%%ds" available-width) ""))
              right))))

;;; Theming

(defun chip-modeline--propertize-octicon (name)
  (propertize (all-the-icons-octicon name)
              'face `(:family ,(all-the-icons-octicon-family) :height 1.0)
              'display '(raise -0.05)))

(defun chip-modeline--propertize-evil-state (s)
  (cond
   ((evil-insert-state-p) (propertize s 'face 'chip-face-evil-state-insert))
   ((evil-emacs-state-p) (propertize s 'face 'chip-face-evil-state-emacs))
   ((evil-visual-state-p) (propertize s 'face 'chip-face-evil-state-visual))
   (t (propertize s 'face 'chip-face-evil-state-normal))))

;;; Modeline

(defun chip-modeline-fallback ()
  "Modeline used as a fallback if no other modeline is available"
  (chip-modeline-format
   '(chip-modeline-tag-evil-state
     chip-modeline-tag-major-mode
     "  "
     chip-modeline-tag-vc
     chip-modeline-misc-info
     chip-modeline-process-info
     "%e ")
   '(chip-modeline-minor-modes
     "  ")))

;;; Tags

(defun chip-modeline-tag-major-mode ()
  "Tag used to display the current major mode"
  (concat
   (chip-modeline--propertize-octicon "code")
   " "
   (cl-case major-mode
     ('emacs-lisp-mode "elisp")
     ('org-agenda-mode "agenda")
     (t (s-chop-suffix "-mode" (symbol-name major-mode))))))

(defun chip-modeline-tag-vc ()
  "Tag used to display the current VC branch"
  (if vc-mode
      (let ((backend (vc-backend buffer-file-name)))
        (concat
         (chip-modeline--propertize-octicon "git-branch")
         " "
         (substring-no-properties vc-mode (+ (if (eq backend 'Hg) 2 3) 2))))
    ""))

(defun chip-modeline-tag-evil-state ()
  "Tag used to display which evil state the buffer is in"
  (if (featurep 'evil)
      (chip-modeline--propertize-evil-state "▌ ")
    " "))

;; (defun chip-modeline-tag-org-clock ()
;;   (concat " | " (all-the-icons-material "timer")
;;           (substring-no-properties org-mode-line-string)))

(defun chip-modeline-misc-info ()
  (let ((content (s-trim-left (format-mode-line mode-line-misc-info))))
    (if (s-blank-str? content)
        ""
      (concat " | " content))))

(defun chip-modeline-process-info ()
  mode-line-process)

(defun chip-modeline-minor-modes ()
  minor-mode-alist)

;;; Configuration

(setq-default mode-line-format '((:eval (chip-modeline-fallback))))

;; taken from:
;; https://www.masteringemacs.org/article/hiding-replacing-modeline-strings
(defvar mode-line-cleaner-alist
  `((lisp-interaction-mode . "lisp-interaction")
    (emacs-lisp-mode . "elisp")
    (magit-status-mode . "magit")
    (org-mode . "org")
    (messages-buffer-mode . "messages"))
  "Alist for `clean-mode-line'.

When you add a new element to the alist, keep in mind that you
must pass the correct minor/major mode symbol and a string you
want to use in the modeline *in lieu of* the original.")

(defun clean-mode-line ()
  (interactive)
  (cl-loop for cleaner in mode-line-cleaner-alist
        do (let* ((mode (car cleaner))
                  (mode-str (cdr cleaner))
                  (old-mode-str (cdr (assq mode minor-mode-alist))))
             (when old-mode-str
               (setcar old-mode-str mode-str))
             ;; major mode
             (when (eq mode major-mode)
               (setq mode-name mode-str)))))

(add-hook 'after-change-major-mode-hook 'clean-mode-line)

;; (use-package telephone-line
;;   :after (evil)
;;   :config
;;   (setq telephone-line-lhs
;;         '((evil   . (telephone-line-evil-tag-segment))
;;           (accent . (telephone-line-vc-segment))
;;           (nil    . (telephone-line-process-segment
;;                      telephone-line-minor-mode-segment))
;; 	  ))
;;   (setq telephone-line-rhs
;;         '((nil    . (telephone-line-erc-modified-channels-segment))
;;           (nil    . (telephone-line-misc-info-segment))
;;           (nil . (telephone-line-major-mode-segment))
;;           (evil   . (telephone-line-airline-position-segment))))
;;   (telephone-line-mode t))


;; I try to diminish most minor modes to keep the modeline free. Most are removed
;; entirely, and some are shortened.

(use-package diminish
  :after (ivy projectile evil-snipe evil-lispy org-roam company-box)
  :config
  (diminish 'auto-fill-function)
  (diminish 'undo-tree-mode)
  (diminish 'company-mode)
  (diminish 'counsel-mode)
  (diminish 'ivy-mode)
  (diminish 'projectile-mode)
  (diminish 'projectile-mode)
  (diminish 'evil-snipe-mode)
  (diminish 'evil-snipe-local-mode)
  (diminish 'lispy-mode)
  (diminish 'evil-lispy-mode)
  (diminish 'auto-revert-mode (chip-modeline--propertize-octicon "sync"))
  (diminish 'emacs-lisp-mode "elisp")
  (diminish 'eldoc-mode)
  (diminish 'yas-minor-mode)
  (diminish 'evil-org-mode)
  (diminish 'org-indent-mode)
  (diminish 'org-roam-mode)
  (diminish 'org-src-mode)
  (diminish 'outshine-mode)
  (diminish 'which-key-mode)
  (diminish 'outline-minor-mode)
  (diminish 'slime-autodoc-mode)
  (diminish 'slime-mode "slime")
  (diminish 'company-box-mode)
  (diminish 'flycheck-mode)
  (diminish 'ace-window-mode)
  (diminish 'olivetti-mode)
  (diminish 'visual-line-mode))

(provide 'chip-modeline)

;;; chip-modeline.el ends here
