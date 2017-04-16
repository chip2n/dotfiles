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

;; telephone-line (https://github.com/dbordak/telephone-line)
(use-package telephone-line
  :ensure t)
(setq telephone-line-lhs
        '((evil   . (telephone-line-evil-tag-segment))
          (accent . (telephone-line-vc-segment
                     telephone-line-process-segment))
          (nil    . (telephone-line-minor-mode-segment
                     telephone-line-buffer-segment))))
(setq telephone-line-rhs
        '((nil    . (telephone-line-misc-info-segment))
          (accent . (telephone-line-major-mode-segment))
          (evil   . (telephone-line-airline-position-segment))))

(telephone-line-mode t)

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

;; margin stuff
(set-frame-parameter nil 'internal-border-width 10)
(set-frame-parameter nil 'left-fringe 5)
(set-frame-parameter nil 'right-fringe 10)


;;(use-package smart-mode-line
;;  :ensure t)
;;(setq sml/no-confirm-load-theme t)



;;(deftheme smart-mode-line-chiptheme "Chip theme for smart-mode-line.")
;;
;;(custom-theme-set-faces
;; 'smart-mode-line-chiptheme
;; '(mode-line-buffer-id ((t :inherit sml/filename :foreground nil :background nil))) 
;; '(mode-line-inactive ((t :foreground "gray60" :background "#404045" :inverse-video nil)))
;; '(mode-line     ((t :foreground "gray60" :background "#ff0000" :inverse-video nil)))
;; '(sml/global    ((t :foreground "gray50" :inverse-video nil)))
;; '(sml/modes     ((t :inherit sml/global :foreground "White")))
;; '(sml/filename  ((t :inherit sml/global :foreground "#eab700" :weight bold)))
;; '(sml/prefix    ((t :inherit sml/global :foreground "#bf6000")))
;; '(sml/read-only ((t :inherit sml/not-modified :foreground "DeepSkyBlue")))
;; '(persp-selected-face ((t :foreground "ForestGreen" :inherit sml/filename)))
;; '(helm-candidate-number ((t :foreground nil :background nil :inherit sml/filename))))
;;
;;(setq sml/theme 'chiptheme)
;;(sml/setup)
