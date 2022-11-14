;; Initialize PATH from /etc/paths
(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

(provide 'chip-init-mac)
