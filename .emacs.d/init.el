; package.el
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)

; use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

; evil
(use-package evil
  :ensure t)
(evil-mode 1)

; neotree
(use-package neotree
  :ensure t)
(setq neo-window-width 40)
(define-key evil-normal-state-map (kbd "<backspace>") 'neotree-toggle)
(add-hook 'neotree-mode-hook
	  (lambda ()
	    (define-key evil-normal-state-local-map (kbd "o") 'neotree-enter)))

; helm
(use-package helm
  :ensure t)
(helm-mode 1)
(define-key evil-normal-state-map (kbd "SPC") 'helm-M-x)

; clojure-mode
(use-package clojure-mode
  :ensure t)

; cider
(use-package cider
  :ensure t)
(add-hook 'clojure-mode-hook 'cider-mode)

; all-the-icons
(use-package all-the-icons
  :ensure t)

; highlight the current line
(global-hl-line-mode +1)

; enable line numbering
(global-linum-mode 1)

; doom theme
(use-package doom-themes
  :ensure t)
(load-theme 'doom-one t)
(setq doom-enable-bold t
      doom-enable-italic t)
(require 'doom-neotree)

; Set default font
(set-face-attribute 'default nil
                    :family "Source Code Pro"
                    :height 110
                    :weight 'normal
                    :width 'normal)

; smart-mode-line
;(use-package smart-mode-line
;  :ensure t)
;(use-package smart-mode-line-powerline-theme
;  :ensure t)
;(setq sml/no-confirm-load-theme t)
;(setq sml/theme 'powerline)
;(sml/setup)

; disable gui fluff
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("84d2f9eeb3f82d619ca4bfffe5f157282f4779732f48a5ac1484d94d5ff5b279" default)))
 '(package-selected-packages (quote (nlinum clojure-mode evil))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
