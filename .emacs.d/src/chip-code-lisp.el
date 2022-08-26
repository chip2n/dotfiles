;;; chip-code-lisp.el -*- lexical-binding: t -*-

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

(define-minor-mode c/code-lisp-mode
  "Mode for working with lisp code."
  :lighter nil
  (if c/code-lisp-mode
      (progn
        (if (featurep 'evil-lispy)
            (evil-lispy-mode 1)
          (lispy-mode 1))
        (outshine-mode 1)
        (prettify-symbols-mode 1)
        (c/complete-mode 1))
    (progn
      (if (featurep 'evil-lispy)
          (evil-lispy-mode -1)
        (lispy-mode -1))
      (outshine-mode -1)
      (prettify-symbols-mode -1)
      (c/complete-mode -1))))

;; TODO move these
(add-hook 'lisp-data-mode-hook #'c/code-lisp-mode)
(add-hook 'emacs-lisp-mode-hook #'c/code-lisp-mode)
(add-hook 'ielm-mode-hook #'c/code-lisp-mode)
(add-hook 'clojure-mode-hook #'c/code-lisp-mode)
(add-hook 'cider-repl-mode-hook #'c/code-lisp-mode)
(add-hook 'clojurescript-mode-hook #'c/code-lisp-mode)
(add-hook 'slime-mode-hook #'c/code-lisp-mode)
(add-hook 'slime-repl-mode-hook #'c/code-lisp-mode)
(add-hook 'sly-mode-hook #'c/code-lisp-mode)
(add-hook 'sly-mrepl-mode-hook #'c/code-lisp-mode)
(add-hook 'racket-mode-hook #'c/code-lisp-mode)
(add-hook 'scheme-mode-hook #'c/code-lisp-mode)

(defun c/lisp-comment-sexp-at-point ()
  (interactive)
  (save-excursion
    (evil-lispy/enter-state-left)
    (lispy-comment)
    (lispy-quit))
  (backward-char 3))

(general-define-key
 :keymaps 'c/code-lisp-mode-map
 "C-;" 'c/lisp-comment-sexp-at-point)

;;; Lispy

(use-package lispy
  :disabled t
  :config
  (c/diminish lispy-mode)

  (setq lispy-close-quotes-at-end-p t)
  (general-unbind
    :keymaps '(lispy-mode-map)
    "M-o" ;; used for ace-window
    )
  (general-define-key
   :keymaps '(lispy-mode-map)
   "S" 'lispy-splice))

(when c/config-evil?
  (use-package evil-lispy
    :after (evil lispy)
    :config
    (c/diminish evil-lispy-mode)
    (if (not (member 'lispy evil-highlight-closing-paren-at-point-states))
        (push 'lispy evil-highlight-closing-paren-at-point-states))))

;;; Symex

(use-package symex
  :config
  (c/diminish symex-mode)
  (c/diminish symex-editing-mode)
  ;; Reverse up/down to match my mental model better
  (setq symex--user-evil-keyspec
      '(("j" . symex-go-up)
        ("k" . symex-go-down)
        ("m" . mark-sexp)
        ("C-j" . symex-climb-branch)
        ("C-k" . symex-descend-branch)
        ("M-j" . symex-goto-highest)
        ("M-k" . symex-goto-lowest)

        ;; I use this keybinding for switching windows
        ("C-<tab>" . nil)))

  (symex-initialize)

  ;; Override evaluation function - we're using sly instead of slime
  (defun symex-eval-common-lisp ()
  "Eval last sexp.

Accounts for different point location in evil vs Emacs mode."
  (interactive)
  (sly-eval-last-expression))

  (defun symex-eval-definition-common-lisp ()
    "Eval entire containing definition."
    (interactive)
    (sly-eval-defun))

  (general-define-key
   :states 'normal
   :keymaps 'symex-mode-map
   ;; :keymaps 'emacs-lisp-mode-map
    "(" 'symex-mode-interface)

  (general-define-key
   :states 'insert
   :keymaps 'symex-mode-map
   "C-(" 'symex-mode-interface))

;;; Elisp

(use-package eldoc
  :config
  (c/diminish eldoc-mode))

(defun describe-thing-at-point ()
  "Show the documentation of the Elisp function and variable near point.
This checks in turn:
-- for a function name where point is
-- for a variable name where point is
-- for a surrounding function call
"
  (interactive)
  (let (sym)
    ;; sigh, function-at-point is too clever.  we want only the first half.
    (cond ((setq sym (ignore-errors
                       (with-syntax-table emacs-lisp-mode-syntax-table
                         (save-excursion
                           (or (not (zerop (skip-syntax-backward "_w")))
                               (eq (char-syntax (char-after (point))) ?w)
                               (eq (char-syntax (char-after (point))) ?_)
                               (forward-sexp -1))
                           (skip-chars-forward "`'")
        	           (let ((obj (read (current-buffer))))
                             (and (symbolp obj) (fboundp obj) obj))))))
           (describe-function sym))
          ((setq sym (variable-at-point)) (describe-variable sym))
          ;; now let it operate fully -- i.e. also check the
          ;; surrounding sexp for a function call.
          ((setq sym (function-at-point)) (describe-function sym)))))

;;; Codegen

(defun c/lisp-wrap-let ()
  "Wrap expression at point inside LET."
  (interactive)
  (symex-wrap-round)
  (forward-char)
  (insert "let ")
  (symex-create-round)
  (symex-wrap-round)
  (forward-char 2)
  (save-excursion
    (forward-char 2)
    (insert "\n")
    (symex-tidy))
  (symex-insert-at-beginning))

(defun c/lisp-wrap-with-slots ()
  "Wrap expression at point inside WITH-SLOTS."
  (interactive)
  (symex-wrap-round)
  (forward-char)
  (insert "with-slots ")
  (symex-create-round)
  (forward-char 1)
  (save-excursion
    (forward-char 1)
    (insert "\n")
    (symex-tidy))
  (symex-insert-at-beginning))


(provide 'chip-code-lisp)

;;; chip-code-lisp.el ends here
