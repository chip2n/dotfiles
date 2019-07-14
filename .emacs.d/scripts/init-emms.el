(use-package emms
  :ensure t
  :config
  ;; (emms-all)
  (emms-standard)
  (emms-default-players)
  ;; (setq emms-player-list '(emms-player-vlc))
  ;; (setq emms-source-file-default-directory "~/SHARE1/SSDext4/SSD/Music/")
  ;; (setq emms-info-functions '(emms-info-mp3info))
  )

(defun chip/setup-emms-keybindings ()
  (general-define-key
   :keymaps 'emms-playlist-mode-map
   "j" 'next-line
   "k" 'previous-line))

(provide 'init-emms)
