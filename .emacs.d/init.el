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

(use-package hydra
  :ensure t)

(require 'init-evil)
(require 'init-helm)
(require 'init-neotree)
(require 'init-slack)

(require 'clojure)
(require 'init-haskell)
(require 'init-python)
(require 'yaml)
(require 'markdown)

(require 'init-org)

(use-package groovy-mode
  :ensure t)

;; save session when exiting emacs
(desktop-save-mode 1)

;; projectile
(use-package projectile
  :ensure t)
(projectile-mode)
(use-package helm-projectile
  :ensure t
  :init
  (defvar helm-source-file-not-found
    (helm-build-dummy-source
	"Create file"
      :action 'find-file))
  (add-to-list 'helm-projectile-sources-list helm-source-file-not-found t))
(evil-leader/set-key
  "p" 'helm-projectile
  "P" 'helm-projectile-switch-project)

;; smartparens: https://github.com/Fuco1/smartparens
(use-package smartparens
  :ensure t
  :config
  (progn
    (show-smartparens-global-mode t)))
;;(require 'smartparens-config)
;;(add-hook 'prog-mode-hook 'turn-on-smartparens-strict-mode)

;; magit: https://github.com/magit/magit
(use-package magit
  :ensure t)
(use-package evil-magit
  :ensure t)
(evil-leader/set-key
  "g" 'magit-status)

;; ace-jump-mode
(use-package ace-jump-mode
  :ensure t)
(evil-leader/set-key
  "s" 'ace-jump-word-mode)

;; evil-snipe: https://github.com/hlissner/evil-snipe
(use-package evil-snipe
  :ensure t)
(evil-snipe-override-mode 1)
;; turn off evil-snipe in ranger
(add-hook 'ranger-mode-hook 'turn-off-evil-snipe-override-mode)
;; turn off evil-snipe in magit
(add-hook 'magit-mode-hook 'turn-off-evil-snipe-override-mode)

;; ranger: https://github.com/ralesi/ranger.el
(use-package ranger
  :ensure t)
(evil-leader/set-key
  "r" 'ranger)
;; make ranger the default file browser
(ranger-override-dired-mode t)
;; disable file preview by default
(setq ranger-preview-file nil)
;; hide hidden files by default
(setq ranger-show-hidden nil)

;; diminish-mode
(use-package diminish
  :ensure t)
(diminish 'evil-snipe-local-mode)
(diminish 'auto-revert-mode)
(diminish 'helm-mode)
(diminish 'undo-tree-mode)
(diminish 'company-mode)
(diminish 'projectile-mode)


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

;; splitting windows
(evil-leader/set-key
  "wh" 'split-window-right
  "wl" (lambda () (interactive) (split-window-right) (other-window 1))
  "wk" 'split-window-below
  "wj" (lambda () (interactive) (split-window-below) (other-window 1)))

(evil-leader/set-key
  "x" 'evil-quit
  "X" 'evil-delete-buffer)

(require 'init-theme)
;; -------------------------------------------------------------


;; ---------------------- editor stuff ----------------------

;; utf-8 as default encoding
(set-language-environment "UTF-8")

;; set default comment column to 70
(setq-default comment-column 70)

;; highlight the current line
(global-hl-line-mode +1)

;; show matching parenthesis
(show-paren-mode 1)

;; disable copy to clipboard on selection
(setq x-select-enable-clipboard nil)

;; smooth-scrolling (https://github.com/aspiers/smooth-scrolling)
(use-package smooth-scrolling
  :ensure t)
(setq smooth-scroll-margin 5)
(smooth-scrolling-mode 1)

;; enable line numbering
;;(global-linum-mode 1)

;; ----------------------------------------------------------

;; ---------------------- keybindings ----------------------

;; open init.el
(evil-leader/set-key
  "c" (lambda ()
	(interactive)
	(find-file "~/.emacs.d/init.el"))
  "f" 'helm-find-files)

;; ---------------------------------------------------------

(require 'init-tetris)

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
    (smooth-scrolling telephone-line intero ranger hydra groovy-mode gradle-mode ace-jump-mode evil-snipe kotlin-mode helm-ag pyvenv evil-magit magit markdown-mode yaml-mode evil-surround url-http-extra-headers url-http smartparens haskell-mode buffer-move elscreen slack emacs-slack evil-leader helm-projectile projectile smart-mode-line nlinum clojure-mode evil))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-level-1 ((t (:inherit outline-1 :height 1.0))))
 '(org-level-2 ((t (:inherit outline-2 :height 1.0))))
 '(org-level-3 ((t (:inherit outline-3 :height 1.0))))
 '(org-level-4 ((t (:inherit outline-4 :height 1.0))))
 '(org-level-5 ((t (:inherit outline-5 :height 1.0))))
 '(telephone-line-accent-active ((t (:inherit mode-line :background "#3c404a" :foreground "white"))))
 '(telephone-line-evil-emacs ((t (:inherit telephone-line-evil :background "orchid1"))))
 '(telephone-line-evil-insert ((t (:inherit telephone-line-evil :background "#5972ab"))))
 '(telephone-line-evil-normal ((t (:inherit telephone-line-evil :background "#2f333a"))))
 '(telephone-line-evil-visual ((t (:inherit telephone-line-evil :background "#5972ab")))))
