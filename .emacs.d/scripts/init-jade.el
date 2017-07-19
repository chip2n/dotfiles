(provide 'init-jade)

(use-package jade-mode
  :ensure t)

(add-to-list 'auto-mode-alist (cons (rx ".jade" eos) 'jade-mode))
