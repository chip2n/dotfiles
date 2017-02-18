(provide 'init-neotree)

(use-package neotree
  :ensure t)
(setq neo-window-width 40)
(define-key evil-normal-state-map (kbd "<backspace>") 'neotree-toggle)
(add-hook 'neotree-mode-hook
	  (lambda ()
	    (define-key evil-normal-state-local-map (kbd "o") 'neotree-enter)))
