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
  ""
  :lighter nil
  (if c/code-lisp-mode
      (if (featurep 'evil-lispy)
          (evil-lispy-mode 1)
        (lispy-mode 1))
    (if (featurep 'evil-lispy)
        (evil-lispy-mode -1)
      (lispy-mode -1))))

;; TODO move these
(add-hook 'emacs-lisp-mode-hook #'c/code-lisp-mode)
(add-hook 'clojure-mode-hook #'c/code-lisp-mode)
(add-hook 'cider-repl-mode-hook #'c/code-lisp-mode)
(add-hook 'clojurescript-mode-hook #'c/code-lisp-mode)
(add-hook 'slime-mode-hook #'c/code-lisp-mode)
(add-hook 'slime-repl-mode-hook #'c/code-lisp-mode)
(add-hook 'sly-mode-hook #'c/code-lisp-mode)
(add-hook 'sly-mrepl-mode-hook #'c/code-lisp-mode)
(add-hook 'racket-mode-hook #'c/code-lisp-mode)
(add-hook 'scheme-mode-hook #'c/code-lisp-mode)

(use-package lispy
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

(provide 'chip-code-lisp)

;;; chip-code-lisp.el ends here
