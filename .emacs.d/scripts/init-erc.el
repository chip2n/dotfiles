(provide 'init-erc)

(require 'erc)

(setq erc-prompt " ")
(setq erc-fill-column 90)

;; Keep emacs from recentering erc buffers
(add-to-list 'erc-mode-hook (lambda ()
                              (set (make-local-variable 'scroll-conservatively) 100)))
