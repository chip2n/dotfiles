; load scripts
(add-to-list 'load-path "~/.emacs.d/scripts")

;; ---------------------- package stuff ----------------------
;; package.el
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)

;; use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
;; -----------------------------------------------------------


(require 'private)

(require 'init-evil)
(require 'init-helm)
(require 'init-neotree)

;; slack
(use-package slack
  :commands (slack-start)
  :init
  (setq slack-buffer-emojify t)
  (setq slack-prefer-current-team t)
  :ensure t
  :config
  (slack-register-team
   :name "remente"
   :default t
   :client-id private/slack-client-id-remente
   :client-secret private/slack-client-secret-remente
   :token private/slack-token-remente))

;; projectile
(use-package projectile
  :ensure t)
(use-package helm-projectile
  :ensure t)
(evil-leader/set-key
  "p" 'helm-projectile)

;; smartparens: https://github.com/Fuco1/smartparens
(use-package smartparens
  :ensure t
  :config
  (progn
    (show-smartparens-global-mode t)))
;;(require 'smartparens-config)

(add-hook 'prog-mode-hook 'turn-on-smartparens-strict-mode)

(require 'clojure)
(require 'haskell)


;; ---------------------- interface stuff ----------------------

;; winner-mode
;; enables undo/redo of window changes
(winner-mode)
(evil-leader/set-key
  "h" 'winner-undo
  "l" 'winner-redo)

;; buffer-move
;; rearrange buffers in a more straightforward way
(use-package buffer-move
  :ensure t)
(define-key evil-normal-state-map (kbd "M-h") 'buf-move-left)
(define-key evil-normal-state-map (kbd "M-k") 'buf-move-up)
(define-key evil-normal-state-map (kbd "M-l") 'buf-move-right)
(define-key evil-normal-state-map (kbd "M-j") 'buf-move-down)

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

;; Set default font
(set-face-attribute 'default nil
                    :family "Source Code Pro"
                    :height 110
                    :weight 'normal
                    :width 'normal)

;; disable gui fluff
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; -------------------------------------------------------------


;; ---------------------- editor stuff ----------------------

;; utf-8 as default encoding
(set-language-environment "UTF-8")

;; set default comment column to 70
(setq-default comment-column 70)

;; highlight the current line
(global-hl-line-mode +1)

;; enable line numbering
(global-linum-mode 1)

;; show matching parenthesis
(show-paren-mode 1)

;; ----------------------------------------------------------

;; ---------------------- keybindings ----------------------

;; open init.el
(evil-leader/set-key
  "c" (lambda ()
	(interactive)
	(find-file "~/.emacs.d/init.el"))
  "f" 'helm-find-files)

;; ---------------------------------------------------------

; smart-mode-line
;(use-package smart-mode-line
;  :ensure t)
;(use-package smart-mode-line-powerline-theme
;  :ensure t)
;(setq sml/no-confirm-load-theme t)
;(setq sml/theme 'powerline)
;(sml/setup)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "84d2f9eeb3f82d619ca4bfffe5f157282f4779732f48a5ac1484d94d5ff5b279" default)))
 '(package-selected-packages
   (quote
    (url-http-extra-headers url-http smartparens haskell-mode buffer-move elscreen slack emacs-slack evil-leader helm-projectile projectile smart-mode-line nlinum clojure-mode evil))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
