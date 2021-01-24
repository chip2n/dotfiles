;;; Common Lisp

(use-package slime
  :config
  (setq inferior-lisp-program "/usr/bin/sbcl")
  (setq slime-description-autofocus t)
  (add-hook 'slime-repl-mode-hook 'header-mode)
  (add-hook 'slime-macroexpansion-minor-mode-hook (lambda () (interactive) (evil-motion-state)))
  (slime-setup '(slime-fancy slime-asdf slime-cl-indent slime-company slime-fuzzy))

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
  :after (slime company))

(defun slime-enable-concurrent-hints ()
  (interactive)
  (setf slime-inhibit-pipelining nil))

(defun sly-mrepl-other-window ()
  (interactive)
  (sly-mrepl #'switch-to-buffer-other-window))

;; (use-package sly
;;   :after (company)
;;   :config
;;   (sly-setup '(;; sly-indentation
;;                ))
;;   (add-to-list 'evil-emacs-state-modes 'sly-db-mode)
;;   (setq inferior-lisp-program "/usr/bin/sbcl")
;;   (add-hook 'sly-mode-hook 'company-mode)
;;   (general-define-key
;;    :keymaps 'sly-mode-map
;;    "C-c C-z" #'sly-mrepl-other-window))

(provide 'chip-lang)
