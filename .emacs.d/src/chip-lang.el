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

;;; C++

(use-package cmake-mode)

(setq-default c-basic-offset 4)

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

;;; Lua

(use-package lua-mode)

(provide 'chip-lang)
