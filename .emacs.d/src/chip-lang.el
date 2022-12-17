;; -*- lexical-binding: t -*-

;;; Elisp

;; stolen from: https://emacs.stackexchange.com/a/10233
;; handles indentation for forms starting with keywords, e.g.
;; (:key1 :value1
;;  :key2 :value2)
(defun chip/lisp-indent-function (indent-point state)
  "This function is the normal value of the variable `lisp-indent-function'.
The function `calculate-lisp-indent' calls this to determine
if the arguments of a Lisp function call should be indented specially.
INDENT-POINT is the position at which the line being indented begins.
Point is located at the point to indent under (for default indentation);
STATE is the `parse-partial-sexp' state for that position.
If the current line is in a call to a Lisp function that has a non-nil
property `lisp-indent-function' (or the deprecated `lisp-indent-hook'),
it specifies how to indent.  The property value can be:
* `defun', meaning indent `defun'-style
  \(this is also the case if there is no property and the function
  has a name that begins with \"def\", and three or more arguments);
* an integer N, meaning indent the first N arguments specially
  (like ordinary function arguments), and then indent any further
  arguments like a body;
* a function to call that returns the indentation (or nil).
  `lisp-indent-function' calls this function with the same two arguments
  that it itself received.
This function returns either the indentation to use, or nil if the
Lisp function does not specify a special indentation."
  (let ((normal-indent (current-column))
        (orig-point (point)))
    (goto-char (1+ (elt state 1)))
    (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
    (cond
     ;; car of form doesn't seem to be a symbol, or is a keyword
     ((and (elt state 2)
           (or (not (looking-at "\\sw\\|\\s_"))
               (looking-at ":")))
      (if (not (> (save-excursion (forward-line 1) (point))
                  calculate-lisp-indent-last-sexp))
          (progn (goto-char calculate-lisp-indent-last-sexp)
                 (beginning-of-line)
                 (parse-partial-sexp (point)
                                     calculate-lisp-indent-last-sexp 0 t)))
      ;; Indent under the list or under the first sexp on the same
      ;; line as calculate-lisp-indent-last-sexp.  Note that first
      ;; thing on that line has to be complete sexp since we are
      ;; inside the innermost containing sexp.
      (backward-prefix-chars)
      (current-column))
     ((and (save-excursion
             (goto-char indent-point)
             (skip-syntax-forward " ")
             (not (looking-at ":")))
           (save-excursion
             (goto-char orig-point)
             (looking-at ":")))
      (save-excursion
        (goto-char (+ 2 (elt state 1)))
        (current-column)))
     (t
      (let (#'(buffer-substring (point)
                                (progn (forward-sexp 1) (point)))
            method)
        (setq method (or (function-get (intern-soft function)
                                       'lisp-indent-function)
                         (get (intern-soft function) 'lisp-indent-hook)))
        (cond ((or (eq method 'defun)
                   (and (null method)
                        (> (length function) 3)
                        (string-match "\\`def" function)))
               (lisp-indent-defform state indent-point))
              ((integerp method)
               (lisp-indent-specform method state
                                     indent-point normal-indent))
              (method
               (funcall method indent-point state))))))))

(add-hook 'emacs-lisp-mode-hook
          (lambda () (setq-local lisp-indent-function #'chip/lisp-indent-function)))

(general-define-key
 :keymaps '(emacs-lisp-mode-map lisp-interaction-mode-map)
  "C-c C-c" 'eval-defun)

(use-package macrostep
  :general (:keymaps 'emacs-lisp-mode-map
            "C-c M-e" 'c/macrostep)
  :config
  (defun c/macrostep ()
    (interactive)
    (macrostep-mode 1)
    (macrostep-expand)))

;;; Common Lisp

(defun c/quicklisp-symlink (path)
  (interactive (list (read-directory-name "Select project to symlink: " chip-dev-dir)))
  (make-symbolic-link (file-truename (s-chop-suffix "/" path)) "~/quicklisp/local-projects/"))

(use-package slime
  :disabled t
  :config
  (c/diminish slime-autodoc-mode)
  (c/diminish slime-mode)
  (setq inferior-lisp-program "/usr/bin/sbcl")
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
  :disabled t)

(defun slime-enable-concurrent-hints ()
  (interactive)
  (setf slime-inhibit-pipelining nil))

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
  "Search for a symbol, optionally limitid to a single package."
  (interactive
   (list (sly-read-from-minibuffer "Apropos external symbols: ")
         (sly-read-package-name "Package (blank for all): "
                                nil 'allow-blank)))
  (sly-apropos string t package))

(use-package sly-macrostep
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
  (let* ((system (completing-read "System: " c/sly-init-current--history nil nil (car c/sly-init-current--history) 'c/sly-init-current--history))
         (pkg (format "#:%s" system)))
    (sly-start :program inferior-lisp-program
               :init-function (lambda ()
                                (sly-eval `(quicklisp:quickload ,system))
                                (sly-switch-package pkg)))))

(defun c/sly-mrepl-quickload ()
  (interactive)
  (let ((package (completing-read "Load package: " (sly-eval `(quicklisp:list-local-systems)))))
    (sly-eval `(quicklisp:quickload ,package))))

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

(use-package sly
  :config
  (require 'sly-autoloads)

  (setq inferior-lisp-program "/usr/bin/sbcl")

  ;; Aim to reuse current SLY buffers when opening the inspector and debugger
  ;(add-to-list 'display-buffer-alist '("\\*sly-inspector.*\\*" c/sly-display-buffer))
  ;(add-to-list 'display-buffer-alist '("\\*sly-db.*\\*" c/sly-display-buffer))
  ;(add-to-list 'display-buffer-alist '("\\*sly-mrepl.*\\*" c/sly-display-buffer))

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

;;; Haskell

(use-package lsp-haskell
  :after (lsp-mode lsp-ui)
  :mode "\\.hs\\'"
  :config

  (defun haskell-evil-open-above ()
    (interactive)
    (evil-digit-argument-or-evil-beginning-of-line)
    (haskell-indentation-newline-and-indent)
    (evil-previous-line)
    (haskell-indentation-indent-line)
    (evil-append-line nil))

  (defun haskell-evil-open-below ()
    (interactive)
    (evil-append-line nil)
    (haskell-indentation-newline-and-indent))

  ;; (setq lsp-haskell-process-path-hie "hie-wrapper")
  (add-hook 'haskell-mode-hook #'lsp)
  (add-hook 'haskell-literate-mode-hook #'lsp)
  ;; (add-hook 'haskell-mode-hook 'flycheck-mode)

  ;; Fix indentation issues in evil-mode
  ;; See: https://emacs.stackexchange.com/q/35431/31683
  (after-load (evil)
    (evil-define-key 'normal haskell-mode-map
      "o" 'haskell-evil-open-below
      "O" 'haskell-evil-open-above)))

;;; Julia

(use-package julia-mode
  :mode "\\.jl\\'")

;;; Swift

(use-package swift-mode
  :mode "\\.swift\\'")

(provide 'chip-lang)
