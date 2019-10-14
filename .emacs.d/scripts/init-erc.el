(provide 'init-erc)

(require 'erc)

(setq erc-prompt " ")
(setq erc-fill-column 90)
;(setq erc-header-line-format (chip/create-header "%t"))

;; (setq erc-autojoin-channels-alist
;;       '(("freenode.net" "#emacs" "#lisp")))

(setq erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"
                                "324" "329" "332" "333" "353" "477"))
(setq erc-track-exclude-server-buffer t)

;; Hide join/part/quit messages from users who have been idle for over an hour
(setq erc-lurker-hide-list '("JOIN" "PART" "QUIT"))
(setq erc-lurker-threshold-time 3600)


;; Keep emacs from recentering erc buffers
(add-to-list 'erc-mode-hook (lambda ()
                              (display-line-numbers-mode -1)
                              (set (make-local-variable 'scroll-conservatively) 100)))

(defun chip/run-erc ()
  (interactive)
  (with-pass (pwd "chat/znc")
    (erc :server private/znc-server
         :port private/znc-port
         :password (format "chip2n:%s" pwd))))
