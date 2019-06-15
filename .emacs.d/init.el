(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(package-initialize)

(setenv "NODE_PATH" "/usr/lib/node_modules")

(add-to-list 'load-path "~/.emacs.d/scripts")

(require 'private)

;; (require 'init-exwm)
(require 'init-use-package)
(require 'init-keybindings)
(require 'init-theme)
(require 'init-evil)
(require 'init-shell)
(require 'init-eshell)
(require 'init-company)
(require 'init-ivy)
(require 'init-avy)
(require 'init-hydra)
(require 'init-magit)
(require 'init-erc)
(require 'init-elfeed)
(require 'init-slack)
(require 'init-projectile)
(require 'init-org)
(require 'init-org-reveal)
(require 'init-eyebrowse)
(require 'init-yasnippet)
(require 'init-diminish)
(require 'init-snipe)
(require 'init-markdown)
(require 'init-yaml)
(require 'init-toml)
(require 'init-lsp)
(require 'init-haskell)
(require 'init-javascript)
(require 'init-dart)
(require 'init-python)
(require 'init-csharp)
(require 'init-kotlin)
(require 'init-groovy)
(require 'init-jade)
(require 'init-clojure)
(require 'init-lisp)
(require 'init-lispy)
(require 'init-racket)
(require 'init-rust)
;; (require 'init-graphviz)
(require 'init-pdf)
(require 'init-pamparam)
(require 'init-pomidor)
;; (require 'init-japanese)
(require 'init-neotree)
(require 'init-google-translate)
(require 'init-quelpa)
(require 'bolt-mode)

;; disable C-d keybinding in comint-mode
;; (enables scrolling in shell, repls etc)
(with-eval-after-load 'comint
    (define-key comint-mode-map "\C-d" nil))

(use-package multi-term
  :ensure t
  :config
  (setq multi-term-program "/bin/bash"))

(use-package ein
  :ensure t
  :config
  (add-to-list 'evil-motion-state-modes 'ein:notebooklist-mode))

(use-package ranger
  :ensure t)
;; make ranger the default file browser
;(ranger-override-dired-mode t)
;; disable file preview by default
(setq ranger-preview-file nil)
;; hide hidden files by default
(setq ranger-show-hidden nil)


;; save backups in separate directory
(setq backup-directory-alist `(("." . "~/.emacs.d/.backups")))
;; save auto saves in separate directory
(setq auto-save-file-name-transforms
      `((".*" "~/.emacs.d/.auto-saves" t)))

;; follow symlinks
(setq vc-follow-symlinks t)

;; disable lock files
(setq create-lockfiles nil) 

;; disable copy to clipboard on selection
(setq select-enable-clipboard nil)

;; indent with spaces by default
(setq-default indent-tabs-mode nil)
(setq-default tabs-width 4)

;; show matching parens
(show-paren-mode 1)
(setq show-paren-delay 0)

;; preserve cursor position when scrolling
(setq scroll-preserve-screen-position t)

;; set default browser to firefox
(setq browse-url-browser-function 'browse-url-firefox)

;; winner-mode
(use-package winner
  :ensure t
  :config
  (winner-mode 1))

;; useful for macros
(defun increment-number-at-point ()
  (interactive)
  (skip-chars-backward "0-9")
  (or (looking-at "[0-9]+")
      (error "No number at point"))
  (replace-match (number-to-string (1+ (string-to-number (match-string 0))))))

;; copy file name of current buffer
(defun chip/copy-file-name-of-buffer ()
  "Put the current file name on the clipboard"
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (with-temp-buffer
        (insert filename)
        (clipboard-kill-region (point-min) (point-max)))
      (message filename))))

;; toggle window dedicated - prevents e.g. magit from replacing them
(defun toggle-window-dedicated ()
  "Toggle whether the current active window is dedicated or not"
  (interactive)
  (message
   (if (let (window (get-buffer-window (current-buffer)))
     (set-window-dedicated-p window
                     (not (window-dedicated-p window))))
       "Window '%s' is dedicated"
     "Window '%s' is normal")
   (current-buffer)))
