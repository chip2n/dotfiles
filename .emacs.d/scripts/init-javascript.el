(provide 'init-javascript)

(setq js-indent-level 2)

;; ignore shebangs
(setq js2-skip-preprocessor-directives t)

(use-package js2-mode
  :ensure t)
(add-to-list 'auto-mode-alist (cons (rx ".js" eos) 'js2-mode))

;; disable semicolon warnings
(setq js2-strict-missing-semi-warning nil)
(setq js2-missing-semi-one-line-override t)

;; disable inconsistent return warnings
(setq js2-strict-inconsistent-return-warning nil)

(use-package json-mode
  :ensure t)
