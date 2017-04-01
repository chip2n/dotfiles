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

(require 'init-evil)
(require 'init-helm)
(require 'init-neotree)
(require 'init-slack)

(require 'clojure)
(require 'init-haskell)
(require 'init-python)
(require 'yaml)
(require 'markdown)

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

(add-hook 'prog-mode-hook 'turn-on-smartparens-strict-mode)

;; magit: https://github.com/magit/magit
(use-package magit
  :ensure t)
(use-package evil-magit
  :ensure t)
(evil-leader/set-key
  "g" 'magit-status)


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
  "wl" 'split-window-right
  "wj" 'split-window-below)

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
    (pyvenv evil-magit magit markdown-mode yaml-mode evil-surround url-http-extra-headers url-http smartparens haskell-mode buffer-move elscreen slack emacs-slack evil-leader helm-projectile projectile smart-mode-line nlinum clojure-mode evil))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
