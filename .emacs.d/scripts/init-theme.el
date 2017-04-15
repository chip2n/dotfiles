(provide 'init-theme)

;; all-the-icons
(use-package all-the-icons
  :ensure t)

;; hide welcome screen
(setq inhibit-startup-screen t)

;; doom theme
(use-package doom-themes
  :ensure t)
(load-theme 'doom-one t)
(setq doom-enable-bold t
      doom-enable-italic t)
(require 'doom-neotree)

;; set default font
(set-face-attribute 'default nil
                    :family "Terminus"
                    :height 110)

;; disable gui fluff
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; disable blinking cursor (caused some flicker in images when using eww)
(blink-cursor-mode 0)

(set-cursor-color "#ffffff")
