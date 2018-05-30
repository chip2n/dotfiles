(provide 'init-pdf)

(use-package pdf-tools
  :ensure t
  :config
  (pdf-tools-install)
  (add-hook 'pdf-view-mode-hook (lambda () (blink-cursor-mode -1)))
  )
