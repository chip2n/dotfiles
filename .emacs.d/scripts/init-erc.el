(provide 'init-erc)

(require 'erc)

(setq erc-prompt " ")
(setq erc-fill-column 90)
;(setq erc-header-line-format (chip/create-header "%t"))

(setq erc-autojoin-channels-alist
      '(("freenode.net" "#emacs" "#lisp")))

;; Keep emacs from recentering erc buffers
(add-to-list 'erc-mode-hook (lambda ()
                              (display-line-numbers-mode -1)
                              (set (make-local-variable 'scroll-conservatively) 100)))
