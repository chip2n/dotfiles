(provide 'init-lisp)

(use-package slime
  :ensure t
  :config
  (setq inferior-lisp-program "/usr/bin/sbcl")
  (setq slime-description-autofocus t)
  (add-hook 'slime-repl-mode-hook 'header-mode)
  (add-hook 'slime-macroexpansion-minor-mode-hook (lambda () (interactive) (evil-motion-state)))
  (slime-setup '(slime-fancy slime-asdf slime-cl-indent slime-company slime-fuzzy)))

(use-package slime-company
  :ensure t
  :after (slime company))

(defun slime-enable-concurrent-hints ()
  (interactive)
  (setf slime-inhibit-pipelining nil))

(defun sly-mrepl-other-window ()
  (interactive)
  (sly-mrepl #'switch-to-buffer-other-window))

;; (use-package sly
;;   :ensure t
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
