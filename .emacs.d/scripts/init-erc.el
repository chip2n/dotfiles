(provide 'init-erc)

(require 'erc)

(setq erc-prompt " ")
(setq erc-fill-column 90)
;(setq erc-header-line-format (chip/create-header "%t"))

(setq erc-autojoin-channels-alist
      '(("freenode.net" "#emacs" "#lisp")))

(setq erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"
                                "324" "329" "332" "333" "353" "477"))
(setq erc-track-exclude-server-buffer t)

;; Keep emacs from recentering erc buffers
(add-to-list 'erc-mode-hook (lambda ()
                              (display-line-numbers-mode -1)
                              (set (make-local-variable 'scroll-conservatively) 100)))
