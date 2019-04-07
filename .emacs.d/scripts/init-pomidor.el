(use-package pomidor
  :ensure t
  :config
  (setq pomidor-sound-tick nil
        pomidor-sound-tack nil
        ;; pomidor-sound-overwork (expand-file-name (concat pomidor-dir "overwork.wav"))
	;; pomidor-sound-break-over (expand-file-name (concat (getenv "HOME") "/Music/overwork.wav"))
        )
  )

(provide 'init-pomidor)
