
;;; package init
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(package-initialize)
(add-to-list 'load-path "~/.emacs.d/scripts")
(require 'init-use-package)

;;; general
(setenv "NODE_PATH" "/usr/lib/node_modules")

;; communication with language servers generate a lot of garbage
(setq gc-cons-threshold 100000000)

;; language servers often generate large responses
(setq read-process-output-max (* 1024 1024))

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

;; scroll line-by-line
(setq-default scroll-up-aggressively 0.0)
(setq-default scroll-down-aggressively 0.0)

(setq scroll-error-top-bottom t)

(setq next-screen-context-lines 4)

;; set default browser to firefox
(setq browse-url-generic-program "brave")
(setq browse-url-browser-function 'browse-url-generic)

(setq select-enable-clipboard t)

(add-hook 'image-mode-hook 'auto-revert-mode)

;;; scripts
(require 'utils)
(require 'private)
(require 'init-pass)
;; (require 'init-exwm)
(require 'init-keybindings)
(require 'init-theme)
(require 'init-autoremote)
(require 'init-evil)
(require 'init-shell)
(require 'init-eshell)
(require 'init-company)
(require 'init-ivy)
(require 'init-prescient)
(require 'init-avy)
(require 'init-hydra)
(require 'init-magit)
(require 'init-dired)
(require 'init-erc)
(require 'init-elfeed)
(require 'init-slack)
(require 'init-projectile)
(require 'init-org)
(require 'init-org-agenda)
(require 'init-org-agenda-doc)
(require 'init-roam)
(require 'init-org-reveal)
(require 'init-yasnippet)
(require 'init-diminish)
(require 'init-snipe)
(require 'init-bash)
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
(require 'init-elisp)
(require 'init-lisp)
(require 'init-lispy)
(require 'init-clojure)
(require 'init-scheme)
(require 'init-rust)
(require 'init-elixir)
(require 'init-android)
(require 'init-glsl)
;; (require 'init-graphviz)
(require 'init-pdf)
(require 'init-pamparam)
(require 'init-pomidor)
(require 'init-lilypond)
(require 'init-compilation)
;; (require 'init-japanese)
(require 'init-neotree)
(require 'init-google-translate)
(require 'init-quelpa)
(require 'init-remente)
(require 'bolt-mode)
;; (require 'vasttrafik)

;;; multi-term
(use-package multi-term
  :ensure t
  :config
  (setq multi-term-program "/bin/bash"))

;;; ranger
(use-package ranger
  :ensure t
  :config
  ;; make ranger the default file browser
  ;; (ranger-override-dired-mode t)

  ;; disable file preview by default
  (setq ranger-preview-file nil)

  ;; hide hidden files by default
  (setq ranger-show-hidden nil))
;;; winner-mode
(use-package winner
  :ensure t
  :config
  (winner-mode 1))

;;; zoom
(use-package zoom
  :ensure t
  :config
  (setq zoom-size '(0.618 . 0.618)))

;;; golden-ratio-scroll-screen
(use-package golden-ratio-scroll-screen
  :ensure t
  :config
  (setq golden-ratio-scroll-highlight-flag nil))

;;; which-key
(use-package which-key
  :ensure t
  :config
  (which-key-mode))

;;; hl-todo
;; highlight TODOs in comments
(use-package hl-todo
  :ensure t
  :hook ((prog-mode . hl-todo-mode)))

(setq inhibit-startup-echo-area-message "chip")

;;; emms
(use-package emms
  :ensure t
  :config
  (emms-all)
  (emms-default-players))

;;; outshine
(use-package outshine
  :ensure t
  :config
  (general-define-key
   :keymaps '(outshine-mode-map)
   :states '(normal)
   "TAB" 'outshine-cycle
   "<backtab>" 'outshine-cycle-buffer)
  (setq outshine-startup-folded-p t)
  (add-hook 'emacs-lisp-mode-hook 'outshine-mode))

;;; utils
;; useful for macros
(defun increment-number-at-point ()
  (interactive)
  (skip-chars-backward "0-9")
  (or (looking-at "[0-9]+")
      (error "No number at point"))
  (replace-match (number-to-string (1+ (string-to-number (match-string 0))))))

;; TODO input via org calendar
(defun insert-unix-timestamp (d)
  (interactive "nDate: ")
  (if (region-active-p) (delete-region (region-beginning) (region-end)))
  (insert
   (car (split-string (shell-command-to-string (format "timestamp %s" d)) "\n"))))

(defun toggle-window-dedicated ()
  "Control whether or not Emacs is allowed to display another
buffer in current window."
  (interactive)
  (message
   (if (let (window (get-buffer-window (current-buffer)))
         (set-window-dedicated-p window (not (window-dedicated-p window))))
       "%s: Can't touch this!"
     "%s is up for grabs.")
   (current-buffer)))

(defun kill-this-buffer-and-process ()
  "Kill current buffer and associated buffer without prompt."
  (interactive)
  (let ((proc (get-buffer-process (current-buffer))))
    (when (processp proc)
      (message "hey")
      (set-process-query-on-exit-flag proc nil)))
  (set-buffer-modified-p nil)
  (quit-window t))

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

(defun yaml-to-json (start end)
  "Convert yaml in region to JSON"
  (interactive "r")
  (let* ((yaml (buffer-substring-no-properties start end))
         (result (shell-command-to-string (concat "python /home/chip/scripts/yaml2json.py " "\"" yaml "\""))))
    (kill-new result)
    (with-temp-buffer
      (insert result)
      (clipboard-kill-region (point-min) (point-max)))
    result))

;;; custom vars
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(outshine vterm inf-elixir exunit emms hl-todo vdiff olivetti noccur wgrep org-roam writeroom-mode company-prescient ivy-prescient which-key zoom yaml-mode xref-js2 worf vasttrafik use-package toml-mode tide telephone-line tayl stylus-mode ssh-agency speed-type smex slack rustic ranger racket-mode quelpa pyvenv pug-mode pomidor pdf-tools pass paren-face ox-pandoc org-re-reveal org-ql org-pomodoro org-plus-contrib org-noter org-gcal org-bullets omnisharp ob-restclient ob-http nvm neotree multi-term lsp-haskell kotlin-mode json-mode js2-refactor ivy-rich ivy-posframe ivy-pass inf-clojure hyperbole helm-org-rifle groovy-mode google-translate golden-ratio-scroll-screen glsl-mode general geiser forge flycheck-rust find-file-in-project eyebrowse evil-visualstar evil-snipe evil-mc evil-lispy evil-collection elixir-mode elfeed eglot doom-themes diminish deft dart-server dart-mode dad-joke counsel-projectile company-lsp company-anaconda cider cargo all-the-icons ag))
 '(safe-local-variable-values
   '((eval if
           (not
            (featurep 'flux))
           (load "/home/chip/git/cl-game-engine/flux.el"))
     (eval load "/home/chip/git/cl-game-engine/flux.el")
     (projectile-project-run-cmd function slime-run-project))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
