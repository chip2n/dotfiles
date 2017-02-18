(use-package helm
  :ensure t)

(helm-mode 1)

(evil-leader/set-key
  "<SPC>" 'helm-M-x
  "b" 'helm-buffers-list )

(provide 'init-helm)
