(provide 'init-pamparam)

(use-package worf
  :ensure t)
(use-package lispy
  :ensure t)
(require 'pamparam)

(setq pamparam-alist
      '(("/home/chip/Dropbox/org/japanese.org"
         . "/home/chip/git/pamparam-japanese")))
