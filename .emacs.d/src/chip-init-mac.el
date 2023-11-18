;; Initialize PATH from /etc/paths
(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

(setq mac-option-modifier 'meta)
(setq mac-command-modifier 'meta)

;; https://colinxy.github.io/software-installation/2016/09/24/emacs25-easypg-issue.html
(setq epa-pinentry-mode 'loopback)

(provide 'chip-init-mac)
