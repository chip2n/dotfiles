(provide 'init-theme)

;; load main emacs theme
(load-theme 'chip t)

;; margin stuff
(set-frame-parameter nil 'internal-border-width 10)

;; disable gui fluff
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; hide welcome screen
(setq inhibit-startup-screen t)

;; highlight the current line
(global-hl-line-mode +1)

;; setup modeline
(use-package telephone-line
  :ensure t
  :after (evil)
  :config
  (setq telephone-line-lhs
        '((evil   . (telephone-line-evil-tag-segment))
          (accent . (telephone-line-vc-segment
                     telephone-line-process-segment))
          (nil    . (telephone-line-minor-mode-segment))
	  ))
  (setq telephone-line-rhs
        '((nil    . (telephone-line-misc-info-segment))
          (accent . (telephone-line-major-mode-segment))
          (evil   . (telephone-line-airline-position-segment))))
  
  (telephone-line-mode t))

;; set font
(set-frame-font "-*-terminus-*-*-*-*-14-*-*-*-*-*-*-*" nil t)
