(provide 'init-japanese)

(require 'uim)

(add-hook 'pamparam-card-mode (lambda () ((uim-mode))))
