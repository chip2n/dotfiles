(add-hook 'dired-mode-hook 'auto-revert-mode)

;; load dired-x immediately to make keybindings available
(require 'dired-x)

;; show directories before files
(setq dired-listing-switches "-aBhl  --group-directories-first")

(provide 'init-dired)
