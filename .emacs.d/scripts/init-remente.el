(require 'remente nil t)

(if (featurep 'remente)
    (setq remente-path "/home/chip/git/remente-cli")
  (warn "remente.el not in load-path"))

(provide 'init-remente)
