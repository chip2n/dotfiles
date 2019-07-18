(require 'remente nil t)

(if (featurep 'remente)
    (progn
      (setq remente-cli-path "/home/chip/git/remente-cli")
      (setq remente-android-path "/home/chip/git/remente-android"))
  (warn "remente.el not in load-path"))

(provide 'init-remente)
