(provide 'init-theme)

;; load main emacs theme
(load-theme 'chip t)

;; margin stuff
;;(set-frame-parameter nil 'internal-border-width 10)

;; disable gui fluff
(scroll-bar-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)

;; show line numbers
;; (global-display-line-numbers-mode t)

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
        '((nil    . (telephone-line-erc-modified-channels-segment))
          (nil    . (telephone-line-misc-info-segment))
          (accent . (telephone-line-major-mode-segment))
          (evil   . (telephone-line-airline-position-segment))))
  
  (telephone-line-mode t))

;; set font
;; (set-frame-font "-*-terminus-*-*-*-*-14-*-*-*-*-*-*-*" t t)
;; (set-frame-font "Hack 11" nil t)
(set-face-attribute 'default nil
        :family "Hack"
        :height 100)

;; Change fringe icons
(define-fringe-bitmap 'left-curly-arrow
  [#b00000000
   #b00000000
   #b00000000
   #b00000000
   #b10000000
   #b11000000
   #b11100000
   #b01110000
   #b00111000
   #b01110000
   #b11100000
   #b11000000
   #b10000000
   #b00000000
   #b00000000
   #b00000000
   #b00000000
   ])
(define-fringe-bitmap 'right-curly-arrow
  [#b00000000
   #b00000000
   #b00000000
   #b00000000
   #b00000001
   #b00000011
   #b00000111
   #b00001110
   #b00011100
   #b00001110
   #b00000111
   #b00000011
   #b00000001
   #b00000000
   #b00000000
   #b00000000
   #b00000000
   ])

(use-package all-the-icons
  :ensure t)

(require 'header-mode)
(setq header-icon (all-the-icons-faicon "codepen" :height 0.8 :v-adjust 0.0))
(add-hook 'find-file-hook 'header-mode)
