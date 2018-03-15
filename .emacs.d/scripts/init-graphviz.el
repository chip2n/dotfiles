(provide 'init-graphviz)

(use-package graphviz-dot-mode
  :ensure t
  :config
  (setq-default graphviz-dot-indent-width 2)
  (add-to-list 'auto-mode-alist (cons (rx ".dot" eos) 'graphviz-dot-mode)))
