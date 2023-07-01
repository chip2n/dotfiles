;; Initialize PATH from /etc/paths
(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

(setq mac-option-modifier 'meta)
(setq mac-command-modifier 'meta)

(provide 'chip-init-mac)
