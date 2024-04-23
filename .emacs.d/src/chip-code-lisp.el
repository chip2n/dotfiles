;;; chip-code-lisp.el -*- lexical-binding: t -*-

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

;;; Paredit

(defun c/paredit-RET-repl-advice (original-function &rest args)
  (cond
   ((eq major-mode 'sly-mrepl-mode) (sly-mrepl-return))
   ((string-equal (buffer-name (current-buffer)) "*ielm*") (ielm-return))
   (t (apply original-function args))))

(use-package paredit
  :bind (:map paredit-mode-map
         ("M-s" . nil))
  :config
  (c/diminish paredit-mode)
  (advice-add 'paredit-RET :around 'c/paredit-RET-repl-advice))

;;; Lispy

(use-package lispy
  :config
  (c/diminish lispy-mode)

  ;; (setq lispy-close-quotes-at-end-p t)
  (setq lispy-colon-p nil)

  (general-unbind
    :keymaps '(lispy-mode-map)
    "M-o" ;; ace-window
    "M-i" ;; completion-at-point
    )
  (general-define-key
   :keymaps '(lispy-mode-map)
   "n" 'special-lispy-down
   "e" 'special-lispy-up
   "h" 'special-lispy-left
   "i" 'special-lispy-right
   "N" 'special-lispy-move-down
   "E" 'special-lispy-move-up
   "S" 'special-lispy-splice
   "v" 'special-lispy-eval
   "V" 'special-lispy-eval-and-insert
   "TAB" 'special-lispy-tab
   "M-(" 'lispy-wrap-round)

  (define-key lispy-mode-map-lispy (kbd "[") nil)
  (define-key lispy-mode-map-lispy (kbd "]") nil)
  (lispy-define-key lispy-mode-map "k" 'lispy-kill-at-point)

  ;; Enable lispy in minibuffer when using eval-expression
  (defun conditionally-enable-lispy ()
    (when (eq this-command 'eval-expression)
      (lispy-mode 1)))
  (add-hook 'minibuffer-setup-hook 'conditionally-enable-lispy))

(when c/config-evil?
  (use-package evil-lispy
    :after (evil lispy)
    :config
    (c/diminish evil-lispy-mode)
    (if (not (member 'lispy evil-highlight-closing-paren-at-point-states))
        (push 'lispy evil-highlight-closing-paren-at-point-states))))

;;; Symex

(use-package symex
  :disabled t
  :after (evil)
  :config
  ;; (c/diminish symex-mode)
  ;; (c/diminish symex-editing-mode)
  ;; Reverse up/down to match my mental model better
  (setq symex--user-evil-keyspec
        '(("j" . symex-go-up)
          ("k" . symex-go-down)
          ;; ("m" . mark-sexp)
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

;;; Custom mode

(define-minor-mode c/code-lisp-mode
  "Mode for working with lisp code."
  :lighter nil
  (require 'outshine)
  (require 'lispy)
  (if c/code-lisp-mode
      (progn
        (if (featurep 'evil-lispy)
            (evil-lispy-mode 1)
          (lispy-mode 1))
        ;; (paredit-mode 1)
        (outshine-mode 1)
        (prettify-symbols-mode 1)
        ;; (c/complete-mode 1)
        )
    (progn
      (if (featurep 'evil-lispy)
          (evil-lispy-mode -1)
        (lispy-mode -1))
      ;; (paredit-mode -1)
      (outshine-mode -1)
      (prettify-symbols-mode -1)
      ;; (c/complete-mode -1)
      )))

;; TODO move these
(add-hook 'lisp-data-mode-hook #'c/code-lisp-mode)
(add-hook 'emacs-lisp-mode-hook #'c/code-lisp-mode)
(add-hook 'ielm-mode-hook #'c/code-lisp-mode)
(add-hook 'clojure-mode-hook #'c/code-lisp-mode)
(add-hook 'cider-repl-mode-hook #'c/code-lisp-mode)
(add-hook 'clojurescript-mode-hook #'c/code-lisp-mode)
(add-hook 'slime-mode-hook #'c/code-lisp-mode)
(add-hook 'slime-repl-mode-hook #'c/code-lisp-mode)
(add-hook 'sly-mrepl-mode-hook #'c/code-lisp-mode)
(add-hook 'racket-mode-hook #'c/code-lisp-mode)
(add-hook 'scheme-mode-hook #'c/code-lisp-mode)

;; (defun c/lisp-comment-sexp-at-point ()
;;   (interactive)
;;   (save-excursion
;;     (evil-lispy/enter-state-left)
;;     (lispy-comment)
;;     (lispy-quit))
;;   (backward-char 3))

;; (general-define-key
;;  :keymaps 'c/code-lisp-mode-map
;;  "C-;" 'c/lisp-comment-sexp-at-point)

(defun c/quicklisp-symlink (path)
  (interactive (list (read-directory-name "Select project to symlink: " chip-dev-dir)))
  (make-symbolic-link (file-truename (s-chop-suffix "/" path)) "~/quicklisp/local-projects/"))

;;; Sly

(use-package sly
  :config
  (require 'sly-autoloads)
  (sly-symbol-completion-mode -1)
  (setq inferior-lisp-program "sbcl")
  (after-load (sly-mrepl)
    (add-to-list 'sly-mrepl-shortcut-alist '("quickload" . c/sly-mrepl-quickload))
    (add-to-list 'sly-mrepl-shortcut-alist '("quickload & switch" . c/sly-mrepl-quickload-and-switch)))
  (after-load (meow)
    (add-to-list 'meow-mode-state-list '(sly-mrepl-mode . normal))))

(defvar *c/sly-mrepl-prev-window* nil)
;; (defun c/sly-mrepl-toggle ()
;;   (interactive)
;;   (let* ((current-window (selected-window))
;;          (buffer (sly-mrepl--find-create (sly-current-connection)))
;;          (win (get-buffer-window buffer)))

;;     (if (eq (window-buffer current-window) buffer)
;;         (progn
;;           (when (window-live-p *c/sly-mrepl-prev-window*)
;;             (select-window *c/sly-mrepl-prev-window*))
;;           (setq *c/sly-mrepl-prev-window* nil))
;;         (if win
;;             (select-window win)
;;           (switch-to-buffer-other-window buffer))
;;       (setq *c/sly-mrepl-prev-window* current-window))

;;     ;; Prevent repl from being selected by other-window (use keybinding instead)
;;     (set-window-parameter win 'no-other-window t)
;;     ))

(defun c/sly-mrepl--open-bottom-window (buffer)
  (display-buffer-in-side-window buffer '((side . bottom)
                                          (slot . 0)
                                          (dedicated . t)
                                          (window-height . 12)
                                          (window-parameters . ((no-other-window . t))))))

(defun c/sly-mrepl--open-side-window (buffer)
  (display-buffer-in-side-window buffer '((side . right)
                                          (slot . 0)
                                          (dedicated . t)
                                          ;; (preserve-size . '(t . nil))
                                          (window-width . 100)
                                          ;; (window-parameters . ((no-other-window . t)))
                                          )))

(defun c/sly-mrepl--toggle (open-fn)
  (let* ((current-window (selected-window))
         (buffer (save-window-excursion (sly-mrepl--find-create (sly-current-connection))))
         (win (get-buffer-window buffer)))

    (if (eq (window-buffer current-window) buffer)
        (progn
          (when (window-live-p *c/sly-mrepl-prev-window*)
            (select-window *c/sly-mrepl-prev-window*))
          (setq *c/sly-mrepl-prev-window* nil))
      (if win
          (select-window win)
        (funcall open-fn buffer))
      (setq *c/sly-mrepl-prev-window* current-window))))

(defun c/sly-mrepl-toggle ()
  (interactive)
  ;; (c/sly-mrepl--toggle #'c/sly-mrepl--open-bottom-window)
  ;; (c/sly-mrepl--toggle #'c/sly-mrepl--open-side-window)
  (c/sly-mrepl--toggle #'switch-to-buffer-other-window)
  )

(defun c/sly-apropos (string package)
  "Search for a symbol, optionally limited to a single package."
  (interactive
   (list (sly-read-from-minibuffer "Apropos external symbols: ")
         (sly-read-package-name "Package (blank for all): "
                                nil 'allow-blank)))
  (sly-apropos string t package))

(use-package sly-macrostep
  :disabled t
  :config
  (require 'sly-macrostep-autoloads)

  (when c/config-evil?
    (add-hook 'macrostep-mode-hook
              (lambda ()
                (if macrostep-mode
                    (evil-emacs-state)
                  (evil-normal-state))))))

(defun sly-switch-package (package)
  (with-current-buffer (sly-mrepl--find-create (sly-current-connection))
    (sly-mrepl--eval-for-repl `(slynk-mrepl:guess-and-set-package ,package))))

(defun sly-init-with-package (package)
  (cl-flet ((init ()
                  (sly-eval `(quicklisp:quickload ,package))
                  (sly-switch-package package)))
    (if (sly-connected-p)
        (init)
      (sly-start :program inferior-lisp-program :init-function #'init))))

(defvar c/sly-init-current--history nil)

(defun c/sly-init-current ()
  (interactive)
  (let* ((system-dir
          (locate-dominating-file
           (buffer-file-name)
           (lambda (dir)
             (or
              (s-ends-with-p ".asd" dir)
              (and (not (f-file-p dir))
                   (seq-find (lambda (f) (s-ends-with-p ".asd" f)) (directory-files dir)))))))
         (system (f-base (seq-find (lambda (f) (s-ends-with-p ".asd" f)) (directory-files system-dir))))
         (pkg (sly-current-package)))
    (sly-start :program inferior-lisp-program
               :init-function (lambda ()
                                (sly-eval `(quicklisp:quickload ,system))
                                (sly-switch-package pkg)))))

(defun c/sly-mrepl-quickload ()
  (interactive)
  (let ((package (completing-read "Load package: " (sly-eval `(quicklisp:list-local-systems)))))
    (sly-eval `(quicklisp:quickload ,package))))

(defun c/sly-mrepl-quickload-and-switch ()
  (interactive)
  (let ((package (car (c/sly-mrepl-quickload))))
    (sly-switch-package package)))

;; (defun c/sly-display-over-repl (buffer alist)
;;   "Use REPL window to display the buffer if it is open.
;; This is intended to be used as an action function for
;; display-buffer (through display-buffer-alist)."
;;   (let ((repl-buffer (sly-mrepl--find-buffer)))
;;     (when repl-buffer
;;       (when-let ((repl-window (car (get-buffer-window-list repl-buffer))))
;;         (select-window repl-window)
;;         (switch-to-buffer buffer)
;;         repl-window))))

(defun c/sly-display-buffer (buffer alist)
  "Use an existing SLY window to display the buffer.
This is intended to be used as an action function for
display-buffer (through display-buffer-alist)."
  (let* ((repl-buffer (sly-mrepl--find-buffer))
         (repl-window (and repl-buffer (car (get-buffer-window-list repl-buffer))))
         (db-buffer (cl-find-if (lambda (b)
                                  (get-buffer-window-list b))
                                (sly-db-buffers)))
         (db-window (and db-buffer (car (get-buffer-window-list db-buffer))))
         (window-to-use (or repl-window db-window))
         (buffer-to-use (or repl-buffer db-buffer)))
    (when window-to-use
      (select-window window-to-use)
      (switch-to-buffer buffer)
      window-to-use)))

;; Old config - experimenting with a minimal one to fix some bugs
(use-package sly
  :disabled t
  :config
  (require 'sly-autoloads)

  (setq inferior-lisp-program "sbcl")

  ;; Aim to reuse current SLY buffers when opening the inspector and debugger
  ;; (add-to-list 'display-buffer-alist '("\\*sly-inspector.*\\*" c/sly-display-buffer))
  ;; (add-to-list 'display-buffer-alist '("\\*sly-db.*\\*" c/sly-display-buffer))
  ;; (add-to-list 'display-buffer-alist '("\\*sly-mrepl.*\\*" c/sly-display-buffer))

  (after-load (evil)
    (add-to-list 'evil-emacs-state-modes 'sly-db-mode)
    (add-to-list 'evil-emacs-state-modes 'sly-inspector-mode)
    ;; (add-to-list 'evil-emacs-state-modes 'sly-xref-mode)
    (add-to-list 'evil-emacs-state-modes 'sly-stickers--replay-mode)
    (add-to-list 'evil-emacs-state-modes 'sly-trace-dialog-mode)
    (add-hook 'sly-xref-mode-hook 'evil-emacs-state)
    (add-hook 'sly-macroexpansion-minor-mode-hook 'evil-emacs-state)
    (add-hook 'sly-inspector-mode-hook 'evil-emacs-state)

    (evil-add-command-properties #'sly-edit-definition :jump t))

  (after-load (company)
    (add-hook 'sly-mrepl-mode-hook 'company-mode))

  (after-load (symex)
    (add-hook 'sly-mrepl-mode-hook 'symex-mode))

  (add-hook 'sly-mrepl-mode-hook
            (lambda () (add-to-list 'sly-mrepl-shortcut-alist '("quickload" . c/sly-mrepl-quickload))))

  (after-load (lispy)
    (setq lispy-use-sly t))
  (setq org-babel-lisp-eval-fn #'sly-eval)

  ;; Push mark before jumping to definitions so that we can quickly get back with pop-global-mark
  (advice-add 'sly-edit-definition :before (lambda (&rest rest) (push-mark)))

  (general-define-key
   :keymaps 'sly-mode-map
   [remap sly-mrepl] 'c/sly-mrepl-toggle
   "C-x i" 'sly-import-symbol-at-point)

  (general-define-key
   :states '(normal)
   :keymaps 'sly-mode-map
   "gd" 'sly-edit-definition)

  (general-define-key
   :states '(normal)
   :keymaps 'sly-popup-buffer-mode-map
   "q" 'quit-window))

;;; Slime

(use-package slime
  :disabled t
  :config
  (c/diminish slime-autodoc-mode)
  (c/diminish slime-mode)
  (setq inferior-lisp-program "sbcl")
  ;; (setq inferior-lisp-program "/usr/bin/ecl")
  (setq slime-description-autofocus t)
  ;; (add-hook 'slime-repl-mode-hook 'header-mode)
  (add-hook 'slime-macroexpansion-minor-mode-hook (lambda () (interactive) (evil-motion-state)))

  (after-load (slime-company)
    (slime-setup '(slime-fancy slime-asdf slime-cl-indent slime-company slime-fuzzy)))

  (add-hook 'slime-xref-mode-hook (lambda () (interactive) (evil-emacs-state)))

  (general-define-key
   :states 'normal
   :keymaps 'slime-mode-map
   "gd" 'slime-edit-definition
   "M-." 'slime-edit-definition ;; overridden by evil?
   )
  (general-define-key
   :states 'normal
   :keymaps 'slime-popup-buffer-mode-map
   "q" 'slime-inspector-quit)

  (general-define-key
   :states 'normal
   :keymaps 'slime-repl-mode-map
   "gd" 'slime-edit-definition
   "C-c i" 'slime-inspect-presentation-at-point)

  (general-define-key
   :keymaps 'slime-macroexpansion-minor-mode-map
   "m" 'slime-macroexpand-1-inplace
   "u" 'slime-macroexpand-undo
   "g" 'slime-macroexpand-again
   "q" 'slime-inspector-quit))

(use-package slime-company
  :disabled t
  :after (company)
  :config
  (add-to-list 'company-backends #'company-slime)
  (setq slime-company-completion 'fuzzy
        slime-company-after-completion 'slime-company-just-one-space))

(defun slime-enable-concurrent-hints ()
  (interactive)
  (setf slime-inhibit-pipelining nil))

;;; Scheme

(use-package geiser
  :defer t
  :hook ((geiser-repl-mode . lispy-mode))
  :config
  (setq geiser-active-implementations '(guile))
  (setq geiser-default-implementation 'guile)
  (after-load (meow)
    (add-to-list 'meow-mode-state-list '(geiser-repl-mode . normal))))

(defun c/geiser-guile-spawn (file)
  "Start a separate guile process and connect to it."
  (interactive "fFile: ")
  (message file)
  (start-process "guile" "*guile*" "guile" "--listen=1661" (expand-file-name file))
  ;; Need to wait for the REPL server to initialize
  ;; TODO We can probably do better than this...
  (sleep-for 0.5)
  (geiser-connect (geiser-repl--get-impl "Connect to Scheme implementation: ") "localhost" 1661))

(use-package geiser-guile
  :defer t
  :after (geiser))

(use-package geiser-gambit
  :defer t
  :after (geiser))

(use-package racket-mode
  :defer t
  :config
  (add-to-list 'auto-mode-alist (cons (rx ".rkt" eos) 'racket-mode)))

(provide 'chip-code-lisp)

;;; chip-code-lisp.el ends here
