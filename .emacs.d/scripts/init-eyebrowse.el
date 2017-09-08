(provide 'init-eyebrowse)

(use-package eyebrowse
  :ensure t
  :init
  (eyebrowse-mode t))
(evil-leader/set-key
  "w0" 'eyebrowse-switch-to-window-config-0
  "w1" 'eyebrowse-switch-to-window-config-1
  "w2" 'eyebrowse-switch-to-window-config-2
  "w3" 'eyebrowse-switch-to-window-config-3
  "w4" 'eyebrowse-switch-to-window-config-4
  "w5" 'eyebrowse-switch-to-window-config-5
  "w6" 'eyebrowse-switch-to-window-config-6
  "w7" 'eyebrowse-switch-to-window-config-7
  "w8" 'eyebrowse-switch-to-window-config-8
  "w9" 'eyebrowse-switch-to-window-config-9
  "wc" 'eyebrowse-close-window-config
  "wn" 'eyebrowse-rename-window-config)
