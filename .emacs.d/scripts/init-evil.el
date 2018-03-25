(provide 'init-evil)

(use-package evil
  :ensure t
  :init
  (setq evil-want-C-u-scroll t)
  :config
  (evil-mode 1)
  ;; allow cursor to move past last character - useful in lisp for
  ;; evaluating last sexp
  ;; (setq evil-move-cursor-back t)
  (setq evil-move-beyond-eol t)
  )

(use-package evil-surround
  :ensure t
  :after (evil)
  :config
  (global-evil-surround-mode 1))
