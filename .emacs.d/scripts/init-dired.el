(add-hook 'dired-mode-hook 'auto-revert-mode)

;; load dired-x immediately to make keybindings available
(require 'dired-x)

(provide 'init-dired)
