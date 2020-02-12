(provide 'init-javascript)

(use-package nvm
  :ensure t)
(nvm-use "v12.10.0")
;; (nvm-use "v10.5.0")
;; (nvm-use "v8.11.3")

(use-package js2-mode
  :ensure t
  :config
  (setq js-indent-level 2)
  (setq js2-skip-preprocessor-directives t) ; ignore shebangs
  (add-to-list 'auto-mode-alist (cons (rx ".js" eos) 'js2-mode))

  ;; disable semicolon warnings
  (setq js2-strict-missing-semi-warning nil)
  (setq js2-missing-semi-one-line-override t)
  
  ;; disable inconsistent return warnings
  (setq js2-strict-inconsistent-return-warning nil))

(use-package json-mode
  :ensure t)

(use-package js2-refactor
  :ensure t
  :after (js2-mode)
  :config
  (add-hook 'js2-mode-hook #'js2-refactor-mode)
  (js2r-add-keybindings-with-prefix "C-c C-r")
  (define-key js2-mode-map (kbd "C-k") #'js2r-kill))

(use-package xref-js2
  :ensure t
  :after (js2-mode)
  :config
  ;; js-mode (which js2 is based on) binds "M-." which conflicts with xref, so
  ;; unbind it.
  (define-key js-mode-map (kbd "M-.") nil)
  (add-hook 'js2-mode-hook
	    (lambda () (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t))))

(use-package typescript-mode
  :ensure t)

(use-package tide
  :ensure t
  :after typescript-mode
  :config
  ;; aligns annotation to the right hand side
  (setq company-tooltip-align-annotations t)

  ;; formats the buffer before saving
  (add-hook 'before-save-hook 'tide-format-before-save)

  (add-hook 'typescript-mode-hook #'setup-tide-mode))

(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
  (company-mode +1))
