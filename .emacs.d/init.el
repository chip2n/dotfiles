(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(package-initialize)
(add-to-list 'load-path "~/.emacs.d/scripts")

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

(defgroup chip-theme nil
  "Options for my personal theme")

(load-theme 'chip t)

(scroll-bar-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)

;; display underline further away from the text
(setq x-underline-at-descent-line t)

;; hide welcome screen
(setq inhibit-startup-screen t)

;; highlight the current line
(global-hl-line-mode +1)

(use-package paren-face
  :ensure t
  :config
  (global-paren-face-mode)
  (setq paren-face-regexp "[][(){}]"))

(use-package all-the-icons
  :ensure t)

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
(define-fringe-bitmap 'left-arrow
  [#b00000000
   #b00000000
   #b00000000
   #b00000000
   #b00001000
   #b00011000
   #b00111000
   #b01110000
   #b11100000
   #b01110000
   #b00111000
   #b00011000
   #b00001000
   #b00000000
   #b00000000
   #b00000000
   #b00000000
   ])
(define-fringe-bitmap 'right-arrow
  [#b00000000
   #b00000000
   #b00000000
   #b00000000
   #b00010000
   #b00011000
   #b00011100
   #b00001110
   #b00000111
   #b00001110
   #b00011100
   #b00011000
   #b00010000
   #b00000000
   #b00000000
   #b00000000
   #b00000000
   ])

(require 'header-mode)
(setq header-icon " λ")
(add-hook 'find-file-hook 'header-mode)

;; (require 'mode-line+)
;; (mode-line-mode+)

;; taken from:
;; https://www.masteringemacs.org/article/hiding-replacing-modeline-strings
(defvar mode-line-cleaner-alist
  `((lisp-interaction-mode . "lisp-interaction")
    (emacs-lisp-mode . "elisp")
    (magit-status-mode . "magit")
    (org-mode . "org")
    (messages-buffer-mode . "messages"))
  "Alist for `clean-mode-line'.

When you add a new element to the alist, keep in mind that you
must pass the correct minor/major mode symbol and a string you
want to use in the modeline *in lieu of* the original.")

(defun clean-mode-line ()
  (interactive)
  (loop for cleaner in mode-line-cleaner-alist
        do (let* ((mode (car cleaner))
                  (mode-str (cdr cleaner))
                  (old-mode-str (cdr (assq mode minor-mode-alist))))
             (when old-mode-str
               (setcar old-mode-str mode-str))
             ;; major mode
             (when (eq mode major-mode)
               (setq mode-name mode-str)))))

(add-hook 'after-change-major-mode-hook 'clean-mode-line)

(use-package telephone-line
  :ensure t
  :after (evil)
  :config
  (setq telephone-line-lhs
        '((evil   . (telephone-line-evil-tag-segment))
          (accent . (telephone-line-vc-segment))
          (nil    . (telephone-line-process-segment
                     telephone-line-minor-mode-segment))
	  ))
  (setq telephone-line-rhs
        '((nil    . (telephone-line-erc-modified-channels-segment))
          (nil    . (telephone-line-misc-info-segment))
          (nil . (telephone-line-major-mode-segment))
          (evil   . (telephone-line-airline-position-segment))))
  (telephone-line-mode t))

(use-package diminish
  :ensure t
  :after (ivy projectile evil-snipe evil-lispy org-roam)
  :config
  (diminish 'auto-fill-function)
  (diminish 'undo-tree-mode)
  (diminish 'company-mode)
  (diminish 'counsel-mode)
  (diminish 'ivy-mode)
  (diminish 'projectile-mode)
  (diminish 'projectile-mode)
  (diminish 'evil-snipe-mode)
  (diminish 'evil-snipe-local-mode)
  (diminish 'lispy-mode)
  (diminish 'evil-lispy-mode)
  (diminish 'auto-revert-mode "arev")
  (diminish 'emacs-lisp-mode "elisp")
  (diminish 'eldoc-mode)
  (diminish 'yas-minor-mode)
  (diminish 'evil-org-mode)
  (diminish 'org-indent-mode)
  (diminish 'org-roam-mode)
  (diminish 'outshine-mode)
  (diminish 'which-key-mode)
  (diminish 'outline-minor-mode)
  (diminish 'slime-autodoc-mode)
  (diminish 'slime-mode "slime"))

;; load private variables
(require 'private)

(require 'cl)

(setenv "NODE_PATH" "/usr/lib/node_modules")
(setenv "ANDROID_SDK_ROOT" "/home/chip/android/sdk")

;; using bash for shell-command
(setq shell-file-name "/bin/bash") 

;; save backups in separate directory
(setq backup-directory-alist `(("." . "~/.emacs.d/.backups")))

;; save auto saves in separate directory
(setq auto-save-file-name-transforms
      `((".*" "~/.emacs.d/.auto-saves" t)))

;; save custom variables to separate file (not loaded)
(setq custom-file (concat user-emacs-directory "/custom.el"))

;; always follow symlinks without asking
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

;; skip confirmation when killing processes
(setq kill-buffer-query-functions
      (delq 'process-kill-buffer-query-function kill-buffer-query-functions))

(defun chip/move-end-of-line ()
  "Move point to end of line.

If in normal or visual evil state, move one character before end of line
in order to be more consistent with the e command."
  (interactive)
  (move-end-of-line 1)
  (cond ((eq evil-state 'normal) (backward-char 1))))

(defun chip/move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

(with-eval-after-load "general"
  (general-define-key
   :states '(normal insert visual emacs)
   "C-e"   'chip/move-end-of-line
   "C-a"   'chip/move-beginning-of-line
   "C-s"   'avy-goto-char-2
   "C-c s" 'avy-goto-char-2
   "M-s"   'swiper))

(with-eval-after-load "general"
  (general-define-key
   :keymaps '(flymake-mode-map)
   "M-n" 'flymake-goto-next-error
   "M-p" 'flymake-goto-prev-error)

  (general-define-key
   :keymaps '(flycheck-mode-map)
   "M-n" 'flycheck-next-error
   "M-p" 'flycheck-previous-error))

(use-package evil-snipe
  :ensure t
  :after (evil)
  :config
  (setq evil-snipe-scope 'buffer)
  (evil-snipe-mode +1)
  (evil-snipe-override-mode +1)
  ;; turn off evil-snipe in magit
  (add-hook 'magit-mode-hook 'turn-off-evil-snipe-override-mode))

(defun chip/open-config-file ()
  "Open Emacs configuration file"
  (interactive)
  (find-file "~/.emacs.d/init.org"))

(with-eval-after-load "general"
  (general-define-key
   :states '(emacs normal insert visual motion)
   "C-c c" 'chip/open-config-file
   "C-x b" 'counsel-switch-buffer
   "C-f"   'counsel-find-file
   "C-p"   'counsel-projectile-find-file
   "C-x p" 'counsel-projectile-find-file
   "C-x a" 'counsel-projectile-ag
   "C-S-P" 'counsel-projectile-switch-project
   "C-x P" 'counsel-projectile-switch-project
   "C-b"   'counsel-switch-buffer))

(use-package projectile
  :ensure t
  :after (ivy)
  :config
  (add-to-list 'projectile-globally-ignored-directories "*node_modules")
  (setq projectile-enable-caching nil)
  ;; we remove -o flag so that untracked files are not included
  ;; this is mainly so that they don't always appear as the first search
  (setq projectile-git-command "git ls-files -zc --exclude-standard")
  (setq projectile-indexing-method 'alien)
  (setq projectile-completion-system 'ivy)
  (projectile-register-project-type 'shadow-cljs '("shadow-cljs.edn")
                                    :src-dir "src/main"
                                    :test-dir "src/test"
                                    :test-suffix "_test")
  (projectile-mode))

(use-package counsel-projectile
  :ensure t
  :after (counsel projectile))

;; For projectile-ag
(use-package ag
  :ensure t
  :after (projectile))

(use-package treemacs
  :ensure t
  :after (evil)
  :config
  (setq treemacs-show-cursor nil)
  (setq treemacs-indentation 1)
  (setq treemacs-space-between-root-nodes nil)
  (add-hook 'treemacs-mode-hook
            (lambda () (setq tab-width 1)))
  (chip/treemacs-setup-theme)
  (chip/treemacs-setup-keys))

(defun chip/treemacs-setup-keys ()
  (general-define-key
   :states '(normal)
   "<backspace>" 'treemacs
   "S-<backspace>" 'treemacs-select-window
   "M-o" 'ace-window)

  (general-define-key
   :keymaps '(treemacs-mode-map)
   "<backspace>" 'treemacs
   "S-<backspace>" 'treemacs-select-window))

(use-package treemacs-evil
  :ensure t
  :after (evil))

(defgroup chip-theme-treemacs nil
  "Options for my treemacs theme"
  :group 'chip-theme)

(defface chip-theme-treemacs-root-face
  '((t (:inherit treemacs-root-face)))
  "Face used for the root icon."
  :group 'chip-theme-treemacs)

(defface chip-theme-treemacs-file-face
  '((t (:inherit treemacs-file-face)))
  "Face used for the directory and file icons."
  :group 'chip-theme-treemacs)

(defun chip/treemacs-setup-theme ()
  (let ((file-face 'chip-theme-treemacs-file-face)
        (root-face 'chip-theme-treemacs-root-face))
    (cl-flet ((icon (name &key (height 1.0) (v-adjust 0.0) (face file-face))
                    (all-the-icons-octicon name
                                           :height 1.0
                                           :v-adjust -0.1
                                           :face 'root-face)))
      (let ((root-icon (icon "repo" :v-adjust -0.1 :face 'root-face))
            (dir-icon (icon "file-directory"))
            (pkg-icon (icon "package"))
            (tag-icon (icon "tag" :height 0.9))
            (error-icon (icon "flame"))
            (warning-icon (icon "stop"))
            (info-icon (icon "info" :height 0.75 :v-adjust 0.1))
            (media-icon (icon "file-media"))
            (code-icon (icon "file-code"))
            (book-icon (icon "book"))
            (text-icon (icon "file-text"))
            (pdf-icon (icon "file-pdf"))
            (zip-icon (icon "file-zip"))
            (binary-icon (icon "file-binary"))
            (parent-closed-icon "+")
            (parent-opened-icon "-"))
        (treemacs-create-theme "chip"
          :icon-directory "~/.emacs.d/icons"
          :config
          (progn
            (treemacs-create-icon
             :icon (format " %s\t" root-icon)
             :extensions (root))
            (treemacs-create-icon
             :icon (format "%s\t%s\t" parent-opened-icon dir-icon)
             :extensions (dir-open))
            (treemacs-create-icon
             :icon (format "%s\t%s\t" parent-closed-icon dir-icon)
             :extensions (dir-closed))
            (treemacs-create-icon
             :icon (format "%s\t%s\t" parent-opened-icon pkg-icon)
             :extensions (tag-open))
            (treemacs-create-icon
             :icon (format "%s\t%s\t" parent-closed-icon pkg-icon)
             :extensions (tag-closed))
            (treemacs-create-icon
             :icon (format "\t\t%s\t" tag-icon)
             :extensions (tag-leaf))
            (treemacs-create-icon
             :icon (format "%s\t" error-icon)
             :extensions (error))
            (treemacs-create-icon
             :icon (format "%s\t" warning-icon)
             :extensions (warning))
            (treemacs-create-icon
             :icon (format "%s\t" info-icon)
             :extensions (info))
            (treemacs-create-icon
             :icon (format "  %s\t" media-icon)
             :extensions ("png" "jpg" "jpeg" "gif" "ico" "tif" "tiff" "svg" "bmp"
                          "psd" "ai" "eps" "indd" "mov" "avi" "mp4" "webm" "mkv"
                          "wav" "mp3" "ogg" "midi"))
            (treemacs-create-icon
             :icon (format "  %s\t" code-icon)
             :extensions ("adoc" "asciidoc" "bashrc" "c" "cabal" "cabal" "cask" "cc"
                          "clj" "cljc" "cljs" "cpp" "css" "csv" "cxx" "dart"
                          "dockerfile" "dockerfile" "editorconfig" "eex" "el"
                          "elm" "ex" "exs" "fish" "gitconfig" "gitignore" "go" "h"
                          "hh" "hpp" "hs" "htm" "html" "hy" "ideavimrc" "inputrc"
                          "j2" "j2" "java" "jinja2" "jinja2" "jl" "js" "json" "jsx"
                          "kt" "kt" "kts" "lhs" "lisp" "lua" "lua" "makefile" "ml"
                          "mli" "nim" "nim" "nims" "nix" "perl" "pl" "plt" "pm"
                          "pm6" "pp" "pp" "py" "pyc" "r" "racket" "rb" "re" "rei"
                          "rkt" "rktd" "rktl" "rs" "sbt" "scala" "scm" "scrbl"
                          "scribble" "scss" "sh" "sql" "sql" "styles" "sv" "tex"
                          "toml" "tpp" "tridactylrc" "ts" "tsx" "v" "vagrantfile"
                          "vagrantfile" "vh" "vimperatorrc" "vimrc" "vrapperrc"
                          "vue" "xml" "xsl" "yaml" "yml" "zsh" "zshrc"))
            (treemacs-create-icon
             :icon (format "  %s\t" book-icon)
             :extensions ("lrf" "lrx" "cbr" "cbz" "cb7" "cbt" "cba" "chm" "djvu"
                          "doc" "docx" "pdb" "pdb" "fb2" "xeb" "ceb" "inf" "azw"
                          "azw3" "kf8" "kfx" "lit" "prc" "mobi" "exe" "or" "html"
                          "pkg" "opf" "txt" "pdb" "ps" "rtf" "pdg" "xml" "tr2"
                          "tr3" "oxps" "xps"))
            (treemacs-create-icon
             :icon (format "  %s\t" text-icon)
             :extensions ("md" "markdown" "rst" "log" "org" "txt"
                          "CONTRIBUTE" "LICENSE" "README" "CHANGELOG"))
            (treemacs-create-icon
             :icon (format "  %s\t" binary-icon)
             :extensions ("exe" "dll" "obj" "so" "o" "out" "elc"))
            (treemacs-create-icon
             :icon (format "  %s\t" pdf-icon)
             :extensions ("pdf"))
            (treemacs-create-icon
             :icon (format "  %s\t" zip-icon)
             :extensions ("zip" "7z" "tar" "gz" "rar" "tgz"))
            (treemacs-create-icon
             :icon (format "  %s\t" text-icon)
             :extensions (fallback)))))))
  (treemacs-load-theme "chip"))

(add-hook 'dired-mode-hook 'auto-revert-mode)

;; load dired-x immediately to make keybindings available
(require 'dired-x)

;; (use-package dired+
;;   :load-path "packages"
;;   :init
;;   (setq diredp-hide-details-initially-flag nil))

;; show directories before files
(setq dired-listing-switches "-aBhl  --group-directories-first")

(general-define-key
 :states '(normal)
 "+" 'text-scale-increase
 "-" 'text-scale-decrease)

(use-package golden-ratio-scroll-screen
  :ensure t
  :config
  (setq golden-ratio-scroll-highlight-flag nil))

;; highlight TODOs in comments
(use-package hl-todo
  :ensure t
  :hook ((prog-mode . hl-todo-mode)))

(setq inhibit-startup-echo-area-message "chip")

(defun chip/window-zoom ()
  (interactive)
  (zoom))

(defun chip/window-unzoom ()
  (interactive)
  (other-window 1)
  (unwind-protect
      (chip/window-zoom)
    (other-window 1)))

(with-eval-after-load "general"
  (general-define-key
   "C-c ["     'winner-undo
   "C-c ]"     'winner-redo
   "C-x +"     'chip/window-zoom
   "C-x -"     'chip/window-unzoom
   "C-x ="     'balance-windows
   "M-o"       'ace-window
   "S-<next>"  'scroll-other-window
   "S-<prior>" 'scroll-other-window-down))

(use-package winner
  :ensure t
  :config
  (winner-mode 1))

;; Allows you to transpose frames (mainly via ace-window)
(require 'transpose-frame)

(use-package avy
  :ensure t)

(use-package ace-window
  :ensure t
  :after (ivy)
  :config
  (setq aw-dispatch-always t)
  (ivy-add-actions
   'ivy-switch-buffer
   '(("a" ace-window "ace-window")))
  (ivy-add-actions
   'counsel-find-file
   '(("a" ace-window "ace-window")))
  (ivy-add-actions
   'counsel-projectile-find-file
   '(("a" ace-window "ace-window"))))

(use-package zoom
  :ensure t
  :config
  (setq zoom-size '(0.618 . 0.618)))

(use-package ivy
  :ensure t
  :config
  (ivy-mode)
  ;; slim down ivy display
  (setq ivy-count-format ""
        ivy-display-style nil
        ivy-minibuffer-faces nil)

  (define-key ivy-minibuffer-map (kbd "<escape>") 'minibuffer-keyboard-quit))

(use-package ivy-rich
  :ensure t
  :config
  (setq ivy-rich-display-transformers-list
        '(counsel-find-file
          (:columns
           ((ivy-read-file-transformer)
            (ivy-rich-counsel-find-file-truename
             (:face font-lock-comment-face))))
          counsel-M-x
          (:columns
           ((counsel-M-x-transformer
             (:width 40))
            (ivy-rich-counsel-function-docstring
             (:face font-lock-comment-face))))
          counsel-describe-function
          (:columns
           ((counsel-describe-function-transformer
             (:width 40))
            (ivy-rich-counsel-function-docstring
             (:face font-lock-comment-face))))
          counsel-describe-variable
          (:columns
           ((counsel-describe-variable-transformer
             (:width 40))
            (ivy-rich-counsel-variable-docstring
             (:align right :face font-lock-comment-face))))
          package-install
          (:columns
           ((ivy-rich-candidate
             (:width 30))
            (ivy-rich-package-version
             (:width 16 :face font-lock-comment-face))
            (ivy-rich-package-archive-summary
             (:width 7 :face font-lock-comment-face))
            (ivy-rich-package-install-summary
             (:face font-lock-comment-face))))))
  (ivy-rich-mode 1))

(use-package counsel
  :ensure t
  :after (ivy)
  :config
  (counsel-mode))

(use-package swiper
  :ensure t
  :after (ivy))

;; Prescient allows you to filter and automatically sort ivy and company results
;; by frequency. It also enables searching by initialism (e.g. stbow ->
;; switch-to-buffer-other-window).
(use-package prescient
  :ensure t
  :after (counsel)
  :config
  (prescient-persist-mode))

(use-package ivy-prescient
  :ensure t
  :after (counsel)
  :config
  (ivy-prescient-mode))

(use-package company-prescient
  :ensure t
  :after (counsel)
  :config
  (company-prescient-mode))

(use-package which-key
  :ensure t
  :config
  (which-key-mode))

(use-package sudo-edit
  :ensure t)

(general-define-key
 :keymap 'prog-mode-map
 "C-;" 'comment-line)

(use-package lispy
  :ensure t
  :config
  (setq lispy-close-quotes-at-end-p t)
  (general-unbind
    :keymaps '(lispy-mode-map)
    "M-o" ;; used for ace-window
    )
  (general-define-key
   :keymaps '(lispy-mode-map)
   "S" 'lispy-splice))

(use-package evil-lispy
  :ensure t
  :after (evil lispy)
  :config
  (if (not (member 'lispy evil-highlight-closing-paren-at-point-states))
      (push 'lispy evil-highlight-closing-paren-at-point-states))

  (add-hook 'emacs-lisp-mode-hook #'evil-lispy-mode)
  (add-hook 'clojure-mode-hook #'evil-lispy-mode)
  (add-hook 'cider-repl-mode-hook #'evil-lispy-mode)
  (add-hook 'clojurescript-mode-hook #'evil-lispy-mode)
  (add-hook 'slime-mode-hook #'evil-lispy-mode)
  (add-hook 'slime-repl-mode-hook #'evil-lispy-mode)
  (add-hook 'sly-mode-hook #'evil-lispy-mode)
  (add-hook 'sly-mrepl-mode-hook #'evil-lispy-mode)
  (add-hook 'racket-mode-hook #'evil-lispy-mode)
  (add-hook 'scheme-mode-hook #'evil-lispy-mode))

(use-package yasnippet
  :ensure t
  :init
  :config
  (yas-global-mode 1)
  (add-to-list 'yas-snippet-dirs (locate-user-emacs-file "snippets")))

(setq compilation-scroll-output t)
(with-eval-after-load "general"
  (general-define-key
   :keymaps '(compilation-mode-map)
   "k" 'kill-this-buffer-and-process))

(defun chip/company-setup-keys ()
  "Setup keybindings for company mode"
  (interactive)
  (define-key company-active-map (kbd "<return>") nil)
  (define-key company-active-map (kbd "RET") nil)
  (define-key company-active-map (kbd "<tab>") #'company-complete-selection))

(use-package company
  :ensure t
  :config
  (add-hook 'company-mode-hook 'chip/company-setup-keys)
  ;; (setq company-idle-delay nil)
  ;; prevent downcasing when autocompleting
  (setq company-dabbrev-downcase nil)
  (setq evil-complete-next-func 'complete-complete-cycle-next)
  (setq evil-complete-previous-func 'complete-complete-cycle-previous)

  ;; no delay in showing suggestions.
  (setq company-idle-delay 0)

  ;; show suggestions after entering one character.
  (setq company-minimum-prefix-length 1)

  (setq company-selection-wrap-around t))

(defun complete-complete-cycle-next (arg)
  (company-complete-common-or-cycle))

(defun complete-complete-cycle-previous (arg)
  (company-complete-common-or-cycle -1))

;; communication with language servers generate a lot of garbage
(setq gc-cons-threshold 100000000)

;; language servers often generate large responses
(setq read-process-output-max (* 1024 1024))

(use-package lsp-mode
  :ensure t
  :config
  (setq lsp-prefer-flymake nil)
  (setq lsp-eldoc-enable-hover nil)
  (setq lsp-enable-xref nil)
  (setq lsp-enable-completion-at-point nil)
  (setq lsp-ui-sideline-enable nil)
  (setq lsp-ui-doc-enable nil)
  (setq lsp-auto-guess-root t)
  ;; prevent docs in minibuffer
  (setq lsp-signature-auto-activate nil)
  (general-define-key
   :keymaps 'lsp-mode-map
   "C-c C-a" 'lsp-execute-code-action
   "C-c C-c C-f" 'lsp-format-buffer)
  (general-define-key
   :keymaps 'lsp-mode-map
   :states '(normal)
   "gd" 'lsp-find-definition))

(use-package company-lsp
  :ensure t
  :after (company)
  :config
  (setq company-lsp-cache-candidates t
        company-lsp-filter-candidates t))

(use-package evil
  :ensure t
  :init
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll nil)
  (setq evil-want-C-d-scroll t)
  (setq evil-disable-insert-state-bindings t)
  :config
  (evil-mode 1)
  ;; allow cursor to move past last character - useful in lisp for
  ;; evaluating last sexp
  ;; (setq evil-move-cursor-back t)
  (setq evil-move-beyond-eol t)
  (add-to-list 'evil-emacs-state-modes 'image-mode))

(use-package evil-visualstar
  :ensure t
  :after (evil)
  :config
  (global-evil-visualstar-mode))

(use-package evil-collection
  :ensure t
  :after (evil)
  :config
  (evil-collection-init 'dired)
  (evil-collection-init 'cider))

(setq evil-fold-list
      '(((hs-minor-mode)
         :open-all hs-show-all :close-all hs-hide-all :toggle hs-toggle-hiding :open hs-show-block :open-rec nil :close hs-hide-block :close-level my-hs-hide-level)
        ((hide-ifdef-mode)
         :open-all show-ifdefs :close-all hide-ifdefs :toggle nil :open show-ifdef-block :open-rec nil :close hide-ifdef-block)
        ((outline-mode outline-minor-mode org-mode markdown-mode)
         :open-all show-all :close-all
         #[nil "\300\301!\207"
               [hide-sublevels 1]
               2]
         :toggle outline-toggle-children :open
         #[nil "\300 \210\301 \207"
               [show-entry show-children]
               1]
         :open-rec show-subtree :close hide-subtree :close-level hide-leaves)))

;; set org todo keywords
(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "WAIT(w@/!)" "HOLD(h@/!)" "|" "DONE(d)" "KILL(c@)")))

;; add timestamp to completed todos
(setq org-log-done 'time)

;; make sure to not alter TODO state when archiving
(setq org-archive-mark-done nil)

;; create automatic bookmarks for org captures
(setq org-bookmark-names-plist
      '(:last-capture "org:last-capture"))

(setq org-file-apps
         '(("\\.png\\'" . "feh --scale-down \"%s\"")
           ("\\.jpg\\'" . "feh --scale-down \"%s\"")
           (auto-mode . emacs)
           ("\\.mm\\'" . default)
           ("\\.x?html?\\'" . default)
           ("\\.pdf\\'" . default)))

;; Cleanup intermediate files after org export
(setq org-latex-logfiles-extensions '("tex" "spl"))

;; Log state changes into the LOGBOOK drawer
(setq org-log-into-drawer t)

(use-package ob-restclient
  :ensure t)

(use-package org-gcal
  :ensure t
  :after org
  :config
  (setq org-gcal-client-id private/gcal-client-id
        org-gcal-client-secret private/gcal-client-secret
        org-gcal-file-alist `((,private/gcal-calendar-id . "~/org/personal/gcal.org"))))

(use-package ob-http
  :ensure t)

(defun enable-dnd ()
  (interactive)
  (autoremote-send "enable-dnd"))

(defun disable-dnd ()
  (interactive)
  (autoremote-send "disable-dnd"))

(use-package org-pomodoro
  :ensure t
  :commands (org-pomodoro)
  :config
  (setq alert-user-configuration (quote ((((:category . "org-pomodoro")) libnotify nil))))
  (add-hook 'org-pomodoro-started-hook 'enable-dnd)
  (add-hook 'org-pomodoro-finished-hook 'disable-dnd)
  (add-hook 'org-pomodoro-killed-hook 'disable-dnd)
  (setq org-pomodoro-short-break-sound "~/audio/waterdrop.wav")
  (setq org-pomodoro-long-break-sound "~/audio/waterdrop.wav")
  (setq org-pomodoro-finished-sound "~/audio/waterdrop.wav")
  (setq org-pomodoro-killed-sound "~/audio/waterdrop.wav")
  (setq org-pomodoro-overtime-sound "~/audio/waterdrop.wav"))

;; stolen from: https://writequit.org/articles/emacs-org-mode-generate-ids.html#automating-id-creation
(require 'org-id)
(setq org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)

(use-package ox-pandoc
  :ensure t)
(setq org-pandoc-options-for-latex-pdf '((pdf-engine . "xelatex")))

(defun org-create-custom-id ()
  (interactive)
  (chip/org-custom-id-get (point) 'create))

(defun chip/org-custom-id-get (&optional pom create prefix)
  "Get the CUSTOM_ID property of the entry at point-or-marker POM.
   If POM is nil, refer to the entry at point. If the entry does
   not have an CUSTOM_ID, the function returns nil. However, when
   CREATE is non nil, create a CUSTOM_ID if none is present
   already. PREFIX will be passed through to `org-id-new'. In any
   case, the CUSTOM_ID of the entry is returned."
  (interactive)
  (org-with-point-at pom
    (let ((id (org-entry-get nil "CUSTOM_ID")))
      (cond
       ((and id (stringp id) (string-match "\\S-" id))
        id)
       (create
        (setq id (org-id-new (concat prefix "h")))
        (org-entry-put pom "CUSTOM_ID" id)
        (org-id-add-location id (buffer-file-name (buffer-base-buffer)))
        id)))))

;; enable blocker properties for unnested dependencies
(require 'org-depend)
(setq org-depend-tag-blocked nil)

(with-eval-after-load "org"
  (with-eval-after-load "general"
    (general-define-key
     :keymaps 'org-mode-map
     "M-p" 'org-previous-visible-heading
     "M-n" 'org-next-visible-heading
     "M-k" 'org-move-subtree-up
     "M-j" 'org-move-subtree-down
     "M-l" 'org-metaright
     "M-h" 'org-metaleft
     "M-L" 'org-demote-subtree
     "M-H" 'org-promote-subtree
     "C-M-<return>" 'org-insert-subheading
     "<RET>" 'org-return-indent)

    (general-define-key
     :prefix "C-c"
     "a" 'chip/org-agenda
     "e" 'org-capture
     "o i" 'org-clock-in
     "o o" 'org-clock-out
     "o g" 'org-clock-goto)))

;; enable easy templates
(require 'org-tempo)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)
   (shell . t)
   (js . t)
   (lilypond . t)
   (ditaa . t)
   (restclient . t)
   (scheme . t)
   (emacs-lisp . t)
   (lisp . t)
   (forth . t)
   (http . t)))

;; Enable noweb expansion in all languages
(setq org-babel-default-header-args
      (cons '(:noweb . "yes")
            (assq-delete-all :noweb org-babel-default-header-args)))

(defun my-org-confirm-babel-evaluate (lang body)
  (not (member lang '("python" "bash" "js" "lisp" "lilypond" "ditaa" "restclient" "scheme" "elisp" "emacs-lisp" "forth"))))

(setq org-confirm-babel-evaluate 'my-org-confirm-babel-evaluate)

;; Redisplay inlined images after source block execution
(add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images)

(setq org-startup-indented t)
(setq org-adapt-indentation nil)
(setq org-indent-indentation-per-level 2)

;; Hide emphasis markers for a more readable document
(setq org-hide-emphasis-markers t)

;; prevent org source blocks from being indented
(setq org-edit-src-content-indentation 0)
(setq org-src-preserve-indentation nil)
(setq org-src-tab-acts-natively nil)

(setq org-tags-column -80)

;; resize image according to ATTR_ORG if available
(setq org-image-actual-width nil)

;; add automatic newlines when lines get too long
;; using this instead of word-wrap since it doesn't affect tables
(add-hook 'org-mode-hook (lambda ()
                           (auto-fill-mode)
                           (setq fill-column 80)))

(use-package org-superstar
  :ensure t
  :after org
  :config
  (setq org-superstar-leading-bullet "#")
  (setq org-superstar-headline-bullets-list '("#"))
  (add-hook 'org-mode-hook (lambda () (org-superstar-mode 1))))

(defface org-bullet
  '((t (:inherit (default))))
  "Face used for org-bullets."
  :group 'org-bullets)

;; (use-package org-bullets
;;   :ensure t
;;   :after org
;;   :config
;;   (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
;;   (setq org-bullets-bullet-list '("ʃ"
;;                                   ;; ""
;;                                   ))
;;   (setq org-bullets-face-name 'org-bullet))

(defun get-presentation-path ()
  "Prompt for presentation name via minibuffer and return path."
  (let ((name (read-from-minibuffer "Presentation name: "))
        (date (shell-command-to-string "echo -n $(date +%Y%m%d)")))
    (format "~/org/remente/presentations/%s-%s/presentation.org" date name)))

(add-hook 'org-capture-mode-hook 'evil-insert-state)

(defun get-journal-path ()
  (let ((date (shell-command-to-string "echo -n $(date +%Y-%m-%d)")))
    (find-file (format "~/org/personal/roam/%s.org" date))))

(setq org-capture-templates
      `(("t" "TODO" entry (file "~/org/personal/refile.org")
         "* TODO %?")
        ("j" "Journal")
        ("je" "Entry" entry #'get-journal-path
         "* %?\n%T")
        ("js" "Day summary" entry #'get-journal-path
         "* Day summary\n%T\n%?\n\n%(org-clock-report-today)")
        ("ju" "Supplements" entry #'get-journal-path
         "* Supplements\n%T\n| %? |  |")
        ("w" "Workout")
        ("wa" "Workout A" entry #'get-journal-path
         "
* Workout
%T
| Bulgarian Split Squat    | 3x10 | %?  |
| Bench Press              | 3x10 |   |
| Straight-Legged Deadlift | 3x10 |   |
| Plank                    | 3x10 | - |
" :clock-in t :clock-resume t)
        ("wb" "Workout B" entry #'get-journal-path
         "
* Workout
%T
| Bulgarian Split Squat | 3x10 | %?  |
| Seated Shoulder Press | 3x10 |   |
| Bent Over Row         | 3x10 |   |
| Plank                 | 3x10 | - |
" :clock-in t :clock-resume t)
        ("i" "Idea" entry (file+olp "~/org/personal/ideas.org" "Ideas")
         "* %?" :prepend t)
        ("p" "Remente presentation" entry #',(lambda () (find-file (get-presentation-path)))
         "
#+OPTIONS: num:nil
#+OPTIONS: toc:nil
#+OPTIONS: reveal_title_slide:nil
#+REVEAL_EXTRA_CSS: /home/chip/.emacs.d/presentation.css
#+REVEAL_TRANS: linear
#+REVEAL_THEME: solarized
#+REVEAL_HLEVEL: 2

* %?")
        ("m" "Meeting" entry (file "~/org/personal/refile.org")
         "* DONE Meeting with %? :meeting:\n%U" :clock-in t :clock-resume t)))

;; auto-saving org buffers after certain actions
(defun save-org-buffers (&rest args)
  (org-save-all-org-buffers))

(advice-add 'org-agenda-quit :before 'save-org-buffers)
(advice-add 'org-deadline :after 'save-org-buffers)
(advice-add 'org-refile :after 'save-org-buffers)
(advice-add 'org-schedule :after 'save-org-buffers)
(advice-add 'org-set-tags-command :after 'save-org-buffers)
(advice-add 'org-clock-in :after 'save-org-buffers)
(advice-add 'org-clock-out :after 'save-org-buffers)
(advice-add 'org-todo :after 'save-org-buffers)

(setq org-refile-targets '((nil :maxlevel . 3)
                           (org-agenda-files :maxlevel . 3)))
(setq org-refile-use-outline-path 'file)
(setq org-outline-path-complete-in-steps nil)

(defun chip/verify-refile-target ()
  "Exclude todo keywords with a done state from refile targets"
  (not (member (nth 2 (org-heading-components)) org-done-keywords)))

(setq org-refile-target-verify-function 'chip/verify-refile-target)

;; remove clock entry if total time span is less than one minute
(setq org-clock-out-remove-zero-time-clocks t)

;; set default clock report parameters
(setq org-clock-clocktable-default-properties
      '(:scope agenda :maxlevel 2 :block today :fileskip0 t :compact t))

(defun org-clock-report-today ()
  "Insert clock report for today's date."
  (let* ((today (shell-command-to-string "echo -n $(date +%Y-%m-%d)"))
         (org-clock-clocktable-default-properties
          `(:scope agenda :maxlevel 2 :block ,(make-symbol today) :fileskip0 t :compact t)))
    (org-clock-report)))

;; change look of indentation in clocktables
(defun my-org-clocktable-indent-string (level)
  (if (= level 1)
      ""
    (let ((str "╰"))
      (while (> level 2)
        (setq level (1- level)
              str (concat str "──")))
      (concat str "─> "))))
(advice-add 'org-clocktable-indent-string :override #'my-org-clocktable-indent-string)

(defun chip/org-agenda ()
  (interactive)
  (org-agenda nil "c"))

(defun chip/dashboard ()
  (interactive)
  (delete-other-windows)
  (split-window-horizontally)
  (chip/org-agenda)
  (chip/window-80)
  (window-preserve-size nil t t)
  (switch-to-buffer-other-window "*scratch*")
  (other-window 1))

(require 'org-habit)

(setq org-agenda-files
      '("~/org/personal/personal.org"
        "~/org/personal/refile.org"
        "~/org/remente/remente.org"))

;; keep agenda filters after closing agenda buffer
(setq org-agenda-persistent-filter t)

;; prevent org-agenda from destroying splits
(setq org-agenda-window-setup 'current-window)

;; always start agenda on current day instead of mondays
(setq org-agenda-start-on-weekday nil)

;; show only today as default
(setq org-agenda-span 'day)

(setq org-agenda-sorting-strategy
      '((agenda habit-down time-up todo-state-up priority-down category-keep)
       (todo priority-down category-keep)
       (tags priority-down category-keep)
       (search category-keep)))

;; force child TODOs to be done before parent can be done
(setq org-enforce-todo-dependencies t)

;; force checkboxes to be completed before parent can be done
(setq org-enforce-todo-checkbox-dependencies t)

;; enable use of the RESET_CHECK_BOXES property
(require 'org-checklist)

(setq org-stuck-projects (quote ("" nil nil "")))

(setq org-agenda-tags-column -80)

;; hide separators between agenda blocks
(setq org-agenda-block-separator nil)

;; don't dim blocked tasks
(setq org-agenda-dim-blocked-tasks nil)

;; remove agenda indentation
(setq org-agenda-prefix-format
      '((agenda . "%i%-12:c%?-12t% s")
        (todo . "%i%-12:c")
        (tags . "%i%-12:c")
        (search . "%i%-12:c")))

;; remove header
(setq org-agenda-overriding-header "")

;; format dates in a nicer way
(setq org-agenda-format-date 'chip/org-agenda-format-date-aligned)

(defun chip/org-agenda-format-date-aligned (date)
  "Format a DATE string for display in the daily/weekly agenda.
This function makes sure that dates are aligned for easy reading."
  (require 'cal-iso)
  (let* ((dayname (downcase (calendar-day-name date)))
	 (day (cadr date))
	 (day-of-week (calendar-day-of-week date))
	 (month (car date))
	 (monthname (downcase (calendar-month-name month)))
	 (year (nth 2 date))
	 (iso-week (org-days-to-iso-week
		    (calendar-absolute-from-gregorian date)))
	 (weekyear (cond ((and (= month 1) (>= iso-week 52))
			  (1- year))
			 ((and (= month 12) (<= iso-week 1))
			  (1+ year))
			 (t year)))
	 (weekstring (if (= day-of-week 1)
			 (format " w%02d" iso-week)
		       "")))
    (let* ((lhs dayname)
           (rhs (format "%2d %s %4d" day monthname year))
           (padding (- 80 (length lhs) (length rhs) 2))
           (pad-str (make-string padding ?-))
           (pattern (format "%%s%%%ds" padding)))
      (format "%s %s %s" lhs pad-str rhs))))

;; set agenda icons
(setq org-agenda-scheduled-leaders `("" "(+%1d)"))
(setq org-agenda-deadline-leaders `("(!!)" "(-%1d)" "(+%1d)"))

;; make time grid as wide as the tag column
(setq org-agenda-time-grid
      '((daily today require-timed)
        (800 1000 1200 1400 1600 1800 2000)
        "......" "--------------------------------------------------------"))
(setq org-agenda-current-time-string
      "--------------------------------------------------------")

(defun bh/find-project-task ()
  "Move point to the parent (project) task if any"
  (save-restriction
    (widen)
    (let ((parent-task (save-excursion (org-back-to-heading 'invisible-ok) (point))))
      (while (org-up-heading-safe)
        (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
          (setq parent-task (point))))
      (goto-char parent-task)
      parent-task)))

(defun bh/is-project-p ()
  "Any task with a todo keyword subtask"
  (save-restriction
    (widen)
    (let ((has-subtask)
          (subtree-end (save-excursion (org-end-of-subtree t)))
          (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
      (save-excursion
        (forward-line 1)
        (while (and (not has-subtask)
                    (< (point) subtree-end)
                    (re-search-forward "^\*+ " subtree-end t))
          (when (member (org-get-todo-state) org-todo-keywords-1)
            (setq has-subtask t))))
      (and is-a-task has-subtask))))

(defun bh/is-project-subtree-p ()
  "Any task with a todo keyword that is in a project subtree.
Callers of this function already widen the buffer view."
  (let ((task (save-excursion (org-back-to-heading 'invisible-ok)
                              (point))))
    (save-excursion
      (bh/find-project-task)
      (if (equal (point) task)
          nil
        t))))

(defun bh/is-task-p ()
  "Any task with a todo keyword and no subtask"
  (save-restriction
    (widen)
    (let ((has-subtask)
          (subtree-end (save-excursion (org-end-of-subtree t)))
          (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
      (save-excursion
        (forward-line 1)
        (while (and (not has-subtask)
                    (< (point) subtree-end)
                    (re-search-forward "^\*+ " subtree-end t))
          (when (member (org-get-todo-state) org-todo-keywords-1)
            (setq has-subtask t))))
      (and is-a-task (not has-subtask)))))

(defun bh/is-subproject-p ()
  "Any task which is a subtask of another project"
  (let ((is-subproject)
        (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
    (save-excursion
      (while (and (not is-subproject) (org-up-heading-safe))
        (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
          (setq is-subproject t))))
    (and is-a-task is-subproject)))

(defun bh/list-sublevels-for-projects-indented ()
  "Set org-tags-match-list-sublevels so when restricted to a subtree we list all subtasks.
  This is normally used by skipping functions where this variable is already local to the agenda."
  (if (marker-buffer org-agenda-restrict-begin)
      (setq org-tags-match-list-sublevels 'indented)
    (setq org-tags-match-list-sublevels nil))
  nil)

(defun bh/list-sublevels-for-projects ()
  "Set org-tags-match-list-sublevels so when restricted to a subtree we list all subtasks.
  This is normally used by skipping functions where this variable is already local to the agenda."
  (if (marker-buffer org-agenda-restrict-begin)
      (setq org-tags-match-list-sublevels t)
    (setq org-tags-match-list-sublevels nil))
  nil)

(defvar bh/hide-scheduled-and-waiting-next-tasks t)

(defun bh/toggle-next-task-display ()
  (interactive)
  (setq bh/hide-scheduled-and-waiting-next-tasks (not bh/hide-scheduled-and-waiting-next-tasks))
  (when  (equal major-mode 'org-agenda-mode)
    (org-agenda-redo))
  (message "%s WAIT and SCHEDULED NEXT Tasks" (if bh/hide-scheduled-and-waiting-next-tasks "Hide" "Show")))

(defun bh/skip-stuck-projects ()
  "Skip trees that are not stuck projects"
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (if (bh/is-project-p)
          (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
                 (has-next ))
            (save-excursion
              (forward-line 1)
              (while (and (not has-next) (< (point) subtree-end) (re-search-forward "^\\*+ NEXT " subtree-end t))
                (unless (member "WAIT" (org-get-tags-at))
                  (setq has-next t))))
            (if has-next
                nil
              next-headline)) ; a stuck project, has subtasks but no next task
        nil))))

(defun bh/skip-non-projects ()
  "Skip trees that are not projects"
  (save-restriction
    (widen)
    (let ((subtree-end (save-excursion (org-end-of-subtree t))))
      (cond
       ((bh/is-project-p)
        nil)
       ((and (bh/is-project-subtree-p) (not (bh/is-task-p)))
        nil)
       (t
        subtree-end)))))

(defun bh/skip-non-tasks ()
  "Show non-project tasks.
Skip project and sub-project tasks, habits, and project related tasks."
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (cond
       ((bh/is-task-p)
        nil)
       (t
        next-headline)))))

(defun bh/skip-project-tasks ()
  "Show non-project tasks.
Skip project and sub-project tasks, habits, and project related tasks."
  (save-restriction
    (widen)
    (let* ((subtree-end (save-excursion (org-end-of-subtree t))))
      (cond
       ((bh/is-project-p)
        subtree-end)
       ((org-is-habit-p)
        subtree-end)
       ((bh/is-project-subtree-p)
        subtree-end)
       (t
        nil)))))

(defun bh/skip-non-project-tasks ()
  "Show project tasks.
Skip project and sub-project tasks, habits, and loose non-project tasks."
  (save-restriction
    (widen)
    (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
           (next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (cond
       ((bh/is-project-p)
        next-headline)
       ((org-is-habit-p)
        subtree-end)
       ((and (bh/is-project-subtree-p)
             (member (org-get-todo-state) (list "NEXT")))
        subtree-end)
       ((not (bh/is-project-subtree-p))
        subtree-end)
       (t
        nil)))))

(defun chip/org-agenda-skip-non-archivable-tasks ()
  "Skip trees that are not available for archiving"
  (save-restriction
    (widen)
    ;; Consider only tasks with done todo headings as archivable candidates
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max))))
          (subtree-end (save-excursion (org-end-of-subtree t))))
      (if (member (org-get-todo-state) org-todo-keywords-1)
          (if (member (org-get-todo-state) org-done-keywords)
              (let* ((daynr (string-to-number (format-time-string "%d" (current-time))))
                     (a-month-ago (* 60 60 24 (+ daynr 1)))
                     (last-month (format-time-string "%Y-%m-" (time-subtract (current-time) (seconds-to-time a-month-ago))))
                     (this-month (format-time-string "%Y-%m-" (current-time)))
                     (subtree-is-current (save-excursion
                                           (forward-line 1)
                                           (and (< (point) subtree-end)
                                                (re-search-forward (concat last-month "\\|" this-month) subtree-end t)))))
                (if subtree-is-current
                    subtree-end ; Has a date in this month or last month, skip it
                  nil))  ; available to archive
            (or subtree-end (point-max)))
        next-headline))))

(setq org-agenda-custom-commands
      (quote (("N" "Notes" tags "NOTE"
               ((org-agenda-overriding-header "Notes")
                (org-tags-match-list-sublevels t)))
              ("h" "Habits" tags-todo "STYLE=\"habit\""
               ((org-agenda-overriding-header "Habits")
                (org-agenda-sorting-strategy
                 '(todo-state-down effort-up category-keep))))
              ("c" "Agenda"
               ((agenda "" nil)
                (tags "refile"
                      ((org-agenda-overriding-header "\nrefile -------------------------------------------------------------------------")
                       (org-tags-match-list-sublevels nil)))
                (tags "-REFILE/"
                      ((org-agenda-overriding-header "archive ------------------------------------------------------------------------")
                       (org-agenda-skip-function 'chip/org-agenda-skip-non-archivable-tasks)
                       (org-tags-match-list-sublevels nil)))
                (tags-todo "-REFILE-KILL/!"
                           ((org-agenda-overriding-header
                             (if bh/hide-scheduled-and-waiting-next-tasks
                                 "\ntasks --------------------------------------------------------------------------"
                               "\ntasks (+wait +sched) -----------------------------------------------------------"))
                            (org-agenda-skip-function 'bh/skip-project-tasks)
                            (org-agenda-tags-todo-honor-ignore-options t)
                            (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-todo-ignore-with-date bh/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-sorting-strategy
                             '(category-keep))))
                (tags-todo
                 "-KILL/!"
                           ((org-agenda-overriding-header "\nprojects -----------------------------------------------------------------------")
                            (org-agenda-skip-function 'bh/skip-non-projects)
                            (org-tags-match-list-sublevels 'indented)
                            (org-agenda-sorting-strategy
                             '(category-keep))))
                (tags-todo "-REFILE-KILL/!"
                           ((org-agenda-overriding-header
                             (if bh/hide-scheduled-and-waiting-next-tasks
                                 "subtasks -----------------------------------------------------------------------"
                               "subtasks (+wait +sched) --------------------------------------------------------"))
                            (org-agenda-skip-function 'bh/skip-non-project-tasks)
                            (org-agenda-tags-todo-honor-ignore-options t)
                            (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-todo-ignore-with-date bh/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-sorting-strategy
                             '(category-keep)))))
               nil))))

(general-define-key
 :keymaps 'org-agenda-mode-map
 "RET" 'org-agenda-switch-to
 "j" 'org-agenda-next-line
 "k" 'org-agenda-previous-line)

(general-define-key
 "C-c o n" 'bh/org-todo
 "C-c o w" 'bh/widen)

(setq org-todo-state-tags-triggers
      (quote (("KILL" ("KILL" . t))
              ("WAIT" ("WAIT" . t))
              ("HOLD" ("WAIT") ("hold" . t))
              (done ("WAIT") ("hold"))
              ("TODO" ("WAIT") ("KILL") ("hold"))
              ("NEXT" ("WAIT") ("KILL") ("hold"))
              ("DONE" ("WAIT") ("KILL") ("hold")))))

(defun chip/org-auto-exclude-function (tag)
  "Automatic task exclusion in the agenda with / RET"
  (and (cond
        ((string= tag "hold")
         t))
       (concat "-" tag)))

(setq org-agenda-auto-exclude-function 'chip/org-auto-exclude-function)

;; - T (tasks) for C-c / t on the current buffer
;; - N (narrow) narrows to this task subtree
;; - U (up) narrows to the immediate parent task subtree without moving
;; - P (project) narrows to the parent project subtree without moving
;; - F (file) narrows to the current file or file of the existing restriction

(defun bh/org-todo (arg)
  (interactive "p")
  (if (equal arg 4)
      (save-restriction
        (bh/narrow-to-org-subtree)
        (org-show-todo-tree nil))
    (bh/narrow-to-org-subtree)
    (org-show-todo-tree nil)))

(defun bh/widen ()
  (interactive)
  (if (equal major-mode 'org-agenda-mode)
      (progn
        (org-agenda-remove-restriction-lock)
        (when org-agenda-sticky
          (org-agenda-redo)))
    (widen)))

(add-hook 'org-agenda-mode-hook
          '(lambda () (org-defkey org-agenda-mode-map "W" (lambda () (interactive) (setq bh/hide-scheduled-and-waiting-next-tasks t) (bh/widen))))
          'append)

(defun bh/restrict-to-file-or-follow (arg)
  "Set agenda restriction to 'file or with argument invoke follow mode.
I don't use follow mode very often but I restrict to file all the time
so change the default 'F' binding in the agenda to allow both"
  (interactive "p")
  (if (equal arg 4)
      (org-agenda-follow-mode)
    (widen)
    (bh/set-agenda-restriction-lock 4)
    (org-agenda-redo)
    (beginning-of-buffer)))

(add-hook 'org-agenda-mode-hook
          '(lambda () (org-defkey org-agenda-mode-map "F" 'bh/restrict-to-file-or-follow))
          'append)

(defun bh/narrow-to-org-subtree ()
  (widen)
  (org-narrow-to-subtree)
  (save-restriction
    (org-agenda-set-restriction-lock)))

(defun bh/narrow-to-subtree ()
  (interactive)
  (if (equal major-mode 'org-agenda-mode)
      (progn
        (org-with-point-at (org-get-at-bol 'org-hd-marker)
          (bh/narrow-to-org-subtree))
        (when org-agenda-sticky
          (org-agenda-redo)))
    (bh/narrow-to-org-subtree)))

(add-hook 'org-agenda-mode-hook
          '(lambda () (org-defkey org-agenda-mode-map "N" 'bh/narrow-to-subtree))
          'append)

(defun bh/narrow-up-one-org-level ()
  (widen)
  (save-excursion
    (outline-up-heading 1 'invisible-ok)
    (bh/narrow-to-org-subtree)))

(defun bh/get-pom-from-agenda-restriction-or-point ()
  (or (and (marker-position org-agenda-restrict-begin) org-agenda-restrict-begin)
      (org-get-at-bol 'org-hd-marker)
      (and (equal major-mode 'org-mode) (point))
      org-clock-marker))

(defun bh/narrow-up-one-level ()
  (interactive)
  (if (equal major-mode 'org-agenda-mode)
      (progn
        (org-with-point-at (bh/get-pom-from-agenda-restriction-or-point)
          (bh/narrow-up-one-org-level))
        (org-agenda-redo))
    (bh/narrow-up-one-org-level)))

(add-hook 'org-agenda-mode-hook
          '(lambda () (org-defkey org-agenda-mode-map "U" 'bh/narrow-up-one-level))
          'append)

(defun bh/narrow-to-org-project ()
  (widen)
  (save-excursion
    (bh/find-project-task)
    (bh/narrow-to-org-subtree)))

(defun bh/narrow-to-project ()
  (interactive)
  (if (equal major-mode 'org-agenda-mode)
      (progn
        (org-with-point-at (bh/get-pom-from-agenda-restriction-or-point)
          (bh/narrow-to-org-project)
          (save-excursion
            (bh/find-project-task)
            (org-agenda-set-restriction-lock)))
        (org-agenda-redo)
        (beginning-of-buffer))
    (bh/narrow-to-org-project)
    (save-restriction
      (org-agenda-set-restriction-lock))))

(add-hook 'org-agenda-mode-hook
          '(lambda () (org-defkey org-agenda-mode-map "P" 'bh/narrow-to-project))
          'append)

(defvar bh/project-list nil)

(defun bh/view-next-project ()
  (interactive)
  (let (num-project-left current-project)
    (unless (marker-position org-agenda-restrict-begin)
      (goto-char (point-min))
      ; Clear all of the existing markers on the list
      (while bh/project-list
        (set-marker (pop bh/project-list) nil))
      (re-search-forward "Tasks to Refile")
      (forward-visible-line 1))

    ; Build a new project marker list
    (unless bh/project-list
      (while (< (point) (point-max))
        (while (and (< (point) (point-max))
                    (or (not (org-get-at-bol 'org-hd-marker))
                        (org-with-point-at (org-get-at-bol 'org-hd-marker)
                          (or (not (bh/is-project-p))
                              (bh/is-project-subtree-p)))))
          (forward-visible-line 1))
        (when (< (point) (point-max))
          (add-to-list 'bh/project-list (copy-marker (org-get-at-bol 'org-hd-marker)) 'append))
        (forward-visible-line 1)))

    ; Pop off the first marker on the list and display
    (setq current-project (pop bh/project-list))
    (when current-project
      (org-with-point-at current-project
        (setq bh/hide-scheduled-and-waiting-next-tasks nil)
        (bh/narrow-to-project))
      ; Remove the marker
      (setq current-project nil)
      (org-agenda-redo)
      (beginning-of-buffer)
      (setq num-projects-left (length bh/project-list))
      (if (> num-projects-left 0)
          (message "%s projects left to view" num-projects-left)
        (beginning-of-buffer)
        (setq bh/hide-scheduled-and-waiting-next-tasks t)
        (error "All projects viewed.")))))

(add-hook 'org-agenda-mode-hook
          '(lambda () (org-defkey org-agenda-mode-map "V" 'bh/view-next-project))
          'append)

;; This prevents too many headlines from being folded together when I'm
;; working with collapsed trees.
(setq org-show-entry-below (quote ((default))))

;; =C-c C-x <= turns on the agenda restriction lock for the current
;; subtree.  This keeps your agenda focused on only this subtree.  Alarms
;; and notifications are still active outside the agenda restriction.
;; =C-c C-x >= turns off the agenda restriction lock returning your
;; agenda view back to normal.

;; I have added key bindings for the agenda to allow using =C-c C-x <= in
;; the agenda to set the restriction lock to the current task directly.
;; The following elisp accomplishes this.

(add-hook 'org-agenda-mode-hook
          '(lambda () (org-defkey org-agenda-mode-map "\C-c\C-x<" 'bh/set-agenda-restriction-lock))
          'append)

(defun bh/set-agenda-restriction-lock (arg)
  "Set restriction lock to current task subtree or file if prefix is specified"
  (interactive "p")
  (let* ((pom (bh/get-pom-from-agenda-restriction-or-point))
         (tags (org-with-point-at pom (org-get-tags-at))))
    (let ((restriction-type (if (equal arg 4) 'file 'subtree)))
      (save-restriction
        (cond
         ((and (equal major-mode 'org-agenda-mode) pom)
          (org-with-point-at pom
            (org-agenda-set-restriction-lock restriction-type))
          (org-agenda-redo))
         ((and (equal major-mode 'org-mode) (org-before-first-heading-p))
          (org-agenda-set-restriction-lock 'file))
         (pom
          (org-with-point-at pom
            (org-agenda-set-restriction-lock restriction-type))))))))

;; Limit restriction lock highlighting to the headline only
(setq org-agenda-restriction-lock-highlight-subtree nil)

(defun chip/org-agenda-to-appt ()
  (interactive)
  (setq appt-time-msg-list nil)
  (org-agenda-to-appt))

;; rebuild the reminders everytime the agenda is displayed
(add-hook 'org-finalize-agenda-hook 'chip/org-agenda-to-appt 'append)

;; set up when emacs starts
(chip/org-agenda-to-appt)

;; activate appointments so we get notifications
(appt-activate t)

;; if we leave Emacs running overnight - reset the appointments one minute after midnight
(run-at-time "24:01" nil 'chip/org-agenda-to-appt)

(use-package org-re-reveal
  :ensure t
  :config
  (setq org-re-reveal-root "file:///home/chip/reveal.js")
  (setq org-re-reveal-title-slide nil))

(server-start)
(require 'org-protocol)

(use-package org-roam
  :ensure t
  :hook
  (after-init . org-roam-mode)
  :config
  (setq org-roam-directory "/home/chip/org/personal/roam")
  (setq org-roam-buffer-position 'left)
  (setq org-roam-buffer-width 0.2)
  (setq org-roam-encrypt-files nil)
  (setq org-roam-db-location "/home/chip/.org-roam.db")
  (add-to-list 'evil-emacs-state-modes 'org-roam-backlinks-mode)
  (require 'org-roam-protocol)
  :bind (:map org-roam-mode-map
              (("C-c n l" . org-roam)
               ("C-c n t" . org-roam-today)
               ("C-c n f" . org-roam-find-file))
              :map org-mode-map
              (("C-c n i" . org-roam-insert))))

(use-package org-roam-server
  :ensure t)

(use-package deft
  :ensure t
  :after (org evil)
  :bind
  ("C-c n d" . deft)
  :config
  (setq deft-recursive t)
  (setq deft-use-filter-string-for-filename t)
  (setq deft-default-extension "org")
  (setq deft-directory "/home/chip/org/personal/roam")
  (setq deft-use-filename-as-title nil)
  (setq deft-extensions '("txt" "text" "md" "markdown" "org" "gpg"))
  (add-to-list 'evil-emacs-state-modes 'deft-mode)

  ;; deft matches directory name as well, so we'll fix it by copying the
  ;; deft-filter-match-file function and changing one line.
  ;; see: https://github.com/jrblevin/deft/issues/66
  (defun deft-filter-match-file (file &optional batch)
    "Return FILE if it is a match against the current filter regexp.
If BATCH is non-nil, treat `deft-filter-regexp' as a list and match
all elements."
    (with-temp-buffer
      (insert (file-name-nondirectory file)) ;; only changed this line
      (let ((title (deft-file-title file))
            (contents (if deft-filter-only-filenames "" (deft-file-contents file))))
        (when title (insert title))
        (when contents (insert contents)))
      (if batch
          (if (every (lambda (filter)
                       (goto-char (point-min))
                       (deft-search-forward filter))
                     deft-filter-regexp)
              file)
        (goto-char (point-min))
        (if (deft-search-forward (car deft-filter-regexp))
            file)))))

(defun chip/magit-status-root-dir (dir)
  (magit-status (vc-find-root dir ".git")))

(use-package magit
  :ensure t
  :after (ivy counsel projectile)
  :config
  (add-hook 'magit-mode-hook (lambda () (display-line-numbers-mode -1)))
  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)

  ;; start magit commit buffers in evil insert mode
  (add-hook 'git-commit-mode-hook 'evil-insert-state)

  (general-define-key
   :keymaps '(shell-mode-map)
   "C-x g" 'magit-status)

  (general-define-key
   :states '(normal)
   :keymaps '(magit-blame-mode-map)
   "RET" 'magit-show-commit
   "q" 'magit-blame-quit)

  (ivy-add-actions
   'counsel-projectile-find-file
   '(("v" chip/magit-status-root-dir "magit")))

  (ivy-add-actions
   'counsel-projectile
   '(("v" chip/magit-status-root-dir "magit"))))

(use-package forge
  :ensure t
  :after magit)

(use-package ssh-agency
  :ensure t
  :config
  (setq ssh-agency-keys '("~/.ssh/github")))

(use-package elfeed
  :ensure t
  :config
  (setq shr-inhibit-images t)           ; disable image loading when viewing entries
  (setq elfeed-feeds
        '(("http://feeds.feedburner.com/blogspot/hsDu" android) ; Android Developers Blog
	  ("http://oremacs.com/atom.xml" emacs)                 ; (or emacs)
	  ("http://pragmaticemacs.com/feed/" emacs)             ; Pragmatic Emacs
          ("https://emacsair.me/feed.xml" emacs)
          ("https://protesilaos.com/codelog.xml" emacs)         ; Protesilaos Stavrou blog
          ("http://feeds.bbci.co.uk/news/science_and_environment/rss.xml" news) ; BBC News - Science & Environment
          ("https://www.theverge.com/rss/index.xml" news) ; The Verge
          ("https://defn.io/index.xml" racket)
          "http://techsnuffle.com/feed.xml"
          "https://ultimatemachine.se/articles.xml"))

  (general-define-key
   :states 'normal
   :keymaps '(elfeed-search-mode-map)
   "f" 'elfeed-search-set-filter
   "r" 'elfeed-update
   "o" 'elfeed-search-show-entry
   "q" 'elfeed-kill-buffer)

  (general-define-key
   :states 'normal
   :keymaps '(elfeed-show-mode-map)
   "q" 'elfeed-kill-buffer
   "n" 'elfeed-show-next
   "p" 'elfeed-show-prev))

(use-package emms
  :ensure t
  :config
  (emms-all)
  (emms-default-players))

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

(use-package alert
  :ensure t
  :commands (alert)
  :config
  (setq alert-default-style 'libnotify))

(require 'erc)

(setq erc-prompt " ")
(setq erc-fill-column 90)
;(setq erc-header-line-format (chip/create-header "%t"))

;; (setq erc-autojoin-channels-alist
;;       '(("freenode.net" "#emacs" "#lisp")))

(setq erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"
                                "324" "329" "332" "333" "353" "477"))
(setq erc-track-exclude-server-buffer t)

;; Hide join/part/quit messages from users who have been idle for over an hour
(setq erc-lurker-hide-list '("JOIN" "PART" "QUIT"))
(setq erc-lurker-threshold-time 3600)


;; Keep emacs from recentering erc buffers
(add-to-list 'erc-mode-hook (lambda ()
                              (display-line-numbers-mode -1)
                              (set (make-local-variable 'scroll-conservatively) 100)))

(defun chip/run-erc ()
  (interactive)
  (with-pass (pwd "chat/znc")
    (erc :server private/znc-server
         :port private/znc-port
         :password (format "chip2n:%s" pwd))))

(use-package pdf-tools
  :ensure t
  :config
  (pdf-tools-install)
  (add-hook 'pdf-view-mode-hook (lambda () (blink-cursor-mode -1))))

(use-package pomidor
  :ensure t
  :config
  (setq pomidor-sound-tick nil
        pomidor-sound-tack nil
        ;; pomidor-sound-overwork (expand-file-name (concat pomidor-dir "overwork.wav"))
	;; pomidor-sound-break-over (expand-file-name (concat (getenv "HOME") "/Music/overwork.wav"))
        ))

(general-define-key
 :map 'emacs-lisp-mode-map
 "C-c C-c" 'eval-defun)

(with-eval-after-load "company"
  (add-hook 'emacs-lisp-mode-hook 'company-mode))

(use-package slime
  :ensure t
  :config
  (setq inferior-lisp-program "/usr/bin/sbcl")
  (setq slime-description-autofocus t)
  (add-hook 'slime-repl-mode-hook 'header-mode)
  (add-hook 'slime-macroexpansion-minor-mode-hook (lambda () (interactive) (evil-motion-state)))
  (slime-setup '(slime-fancy slime-asdf slime-cl-indent slime-company slime-fuzzy))

  (add-hook 'slime-xref-mode-hook (lambda () (interactive) (evil-emacs-state)))

  (general-define-key
    :states 'normal
    :keymaps 'slime-mode-map
    "gd" 'slime-edit-definition
    "M-." 'slime-edit-definition ;; overridden by evil?
    )
  (general-define-key
   :states 'normal
   :keymaps 'slime-popup-buffer-mode-map
   "q" 'slime-inspector-quit)

  (general-define-key
   :states 'normal
   :modes 'slime-repl-mode
   "C-c i" 'slime-inspect-presentation-at-point)

  (general-define-key
   :keymaps 'slime-macroexpansion-minor-mode-map
   "m" 'slime-macroexpand-1-inplace
   "u" 'slime-macroexpand-undo
   "q" 'slime-inspector-quit))

(use-package slime-company
  :ensure t
  :after (slime company))

(defun slime-enable-concurrent-hints ()
  (interactive)
  (setf slime-inhibit-pipelining nil))

(defun sly-mrepl-other-window ()
  (interactive)
  (sly-mrepl #'switch-to-buffer-other-window))

;; (use-package sly
;;   :ensure t
;;   :after (company)
;;   :config
;;   (sly-setup '(;; sly-indentation
;;                ))
;;   (add-to-list 'evil-emacs-state-modes 'sly-db-mode)
;;   (setq inferior-lisp-program "/usr/bin/sbcl")
;;   (add-hook 'sly-mode-hook 'company-mode)
;;   (general-define-key
;;    :keymaps 'sly-mode-map
;;    "C-c C-z" #'sly-mrepl-other-window))

(use-package geiser
  :ensure t
  :after (evil)
  :config
  (setq geiser-chicken-binary "chicken-csi")
  (setq geiser-active-implementations '(chicken racket))
  (setq geiser-default-implementation 'chicken)
  (add-hook 'geiser-repl-mode-hook 'evil-lispy-mode)
  (add-to-list 'evil-emacs-state-modes 'geiser-debug-mode))

(use-package racket-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist (cons (rx ".rkt" eos) 'racket-mode)))

(require 'forth-mode)

(use-package clojure-mode
  :ensure t)

(use-package cider
  :ensure t
  :config
  (setq cider-test-show-report-on-success t)
  (eldoc-mode t)
  (add-to-list 'evil-motion-state-modes 'cider-test-report-mode))

(use-package inf-clojure
  :ensure t)

(use-package anaconda-mode
  :ensure t
  :config
  (add-hook 'python-mode-hook 'anaconda-mode))

(use-package pyvenv
  :ensure t)

(use-package company-anaconda
  :ensure t
  :after (company anaconda-mode)
  :config
  (add-to-list 'company-backends 'company-anaconda))

(use-package dart-mode
  :ensure t
  :after (projectile)
  :config
  (add-to-list 'auto-mode-alist (cons (rx ".dart" eos) 'dart-mode))
  ;; (setq dart-enable-analysis-server t)
  (setq dart-sdk-path "/home/chip/flutter/bin/cache/dart-sdk/")
  (add-hook 'dart-mode-hook 'lsp)
  (add-hook 'dart-mode-hook 'flycheck-mode)
  (add-hook 'dart-mode-hook (lambda ()
                              (add-hook 'after-save-hook 'flutter-hot-reload nil 'make-it-local)))
  (add-to-list 'projectile-project-root-files-bottom-up "pubspec.yaml")
  (add-to-list 'projectile-project-root-files-bottom-up "BUILD")

  (general-define-key
   :states 'normal
   :keymaps 'dart-mode-map
   "gd" 'dart-server-goto)

  (general-define-key
   :prefix "C-c"
   :states 'normal
   :keymaps 'dart-mode-map
   "ww" 'flutter-widget-wrap-widget
   "wg" 'flutter-widget-wrap-group
   "wr" 'flutter-widget-lift
   "f" 'dart-server-format))

(use-package dart-server
  :ensure t
  :after (dart-mode))

(defun flutter--find-project-root ()
  (locate-dominating-file (buffer-file-name) "pubspec.yaml"))

(defun flutter-run ()
  (interactive)
  (let ((project-root (flutter--find-project-root)))
    (if (not project-root)
        (error "Not inside a flutter project (no pubspec.yaml found in any parent directory)."))
    (pop-to-buffer (get-buffer-create "*flutter*"))
    (cd project-root)
    (shell (current-buffer))
    (process-send-string nil "flutter run -d all --pid-file /tmp/flutter.pid\n")
    (evil-normal-state)
    (other-window -1)))

(defun flutter-hot-reload ()
  "Triggers a hot reload of the running flutter application"
  (interactive)
  (shell-command "kill -SIGUSR1 $(cat /tmp/flutter.pid)"))

(defun flutter-hot-restart ()
  "Triggers a hot restart of the running flutter application"
  (interactive)
  (shell-command "kill -SIGUSR2 $(cat /tmp/flutter.pid)"))

(defun flutter--move-beginning-of-widget ()
  (re-search-backward (rx space))
  (forward-char 1))

(defun flutter--move-end-of-widget ()
  (forward-list 1))

(defun flutter-select-widget-at-point ()
  (interactive)
  (flutter--move-beginning-of-widget)
  (set-mark-command nil)
  (flutter--move-end-of-widget)
  (setq deactivate-mark nil))

(defun flutter-widget-wrap-padding ()
  (interactive)
  (flutter-select-widget-at-point)
  (yas-expand-snippet (yas-lookup-snippet "padding"))
  (evil-insert 1))

(defun flutter-widget-wrap-center ()
  (interactive)
  (flutter-select-widget-at-point)
  (yas-expand-snippet (yas-lookup-snippet "center"))
  (evil-insert 1))

(defun flutter-widget-wrap-widget ()
  (interactive)
  (flutter-select-widget-at-point)
  (yas-expand-snippet (yas-lookup-snippet "widget"))
  (evil-insert 1))

(defun flutter-widget-wrap-group ()
  (interactive)
  (flutter-select-widget-at-point)
  (yas-expand-snippet (yas-lookup-snippet "group"))
  (evil-insert 1))

(defmacro flutter--widget-execute-region (fun)
  "Run function on region (must take beginning and end as last two arguments"
  `(save-excursion
     (flutter--move-beginning-of-widget)
     (let ((beg (point)))
       (flutter--move-end-of-widget)
       (,fun beg (point)))))

(defun flutter-widget-kill ()
  (interactive)
  (flutter--widget-execute-region kill-region))

(defun flutter-widget-delete ()
  (interactive)
  (flutter--widget-execute-region delete-region))

(defun flutter-widget-copy ()
  (interactive)
  (flutter--widget-execute-region copy-region-as-kill))

(defun flutter--move-beginning-of-parent-widget ()
  (interactive)
  (backward-char 1)
  (re-search-backward (rx (syntax open-parenthesis)))
  (flutter--move-beginning-of-widget))

(defun flutter-widget-lift ()
  (interactive)
  (flutter-widget-copy)
  (flutter--move-beginning-of-parent-widget)
  (flutter-widget-delete)
  (yank))

(use-package lsp-haskell
  :ensure t
  :after (lsp-mode lsp-ui)
  :config
  (setq lsp-haskell-process-path-hie "hie-wrapper")
  (add-hook 'haskell-mode-hook 'lsp-haskell-enable)
  (add-hook 'haskell-mode-hook 'flycheck-mode))

(use-package nvm
  :ensure t)
(nvm-use "v12.10.0")
;; (nvm-use "v8.16.2")

(use-package js2-mode
  :ensure t
  :config
  (setq js-indent-level 2)
  (setq js2-skip-preprocessor-directives t) ; ignore shebangs
  (add-to-list 'auto-mode-alist (cons (rx ".js" eos) 'js2-mode))

  ;; disable semicolon warnings
  (setq js2-strict-missing-semi-warning nil)
  (setq js2-missing-semi-one-line-override t)
  
  ;; disable inconsistent return warnings
  (setq js2-strict-inconsistent-return-warning nil))

(use-package json-mode
  :ensure t)

(use-package js2-refactor
  :ensure t
  :after (js2-mode)
  :config
  (add-hook 'js2-mode-hook #'js2-refactor-mode)
  (js2r-add-keybindings-with-prefix "C-c C-r")
  (define-key js2-mode-map (kbd "C-k") #'js2r-kill))

(use-package xref-js2
  :ensure t
  :after (js2-mode)
  :config
  ;; js-mode (which js2 is based on) binds "M-." which conflicts with xref, so
  ;; unbind it.
  (define-key js-mode-map (kbd "M-.") nil)
  (add-hook 'js2-mode-hook
	    (lambda () (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t))))

(use-package typescript-mode
  :ensure t)

(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  (company-mode +1))

(use-package tide
  :ensure t
  :after (typescript-mode)
  :config
  ;; aligns annotation to the right hand side
  (setq company-tooltip-align-annotations t)
  ;; formats the buffer before saving
  (add-hook 'before-save-hook 'tide-format-before-save)
  (add-hook 'typescript-mode-hook 'setup-tide-mode)
  (general-define-key
   :states 'normal
   :keymaps 'typescript-mode-map
   "gd" 'tide-jump-to-definition))

(use-package omnisharp
  :ensure t
  :after (company)
  :config
  (add-to-list 'company-backends 'company-omnisharp)
  (add-hook 'csharp-mode-hook #'company-mode)
  (add-hook 'csharp-mode-hook 'omnisharp-mode))

(use-package kotlin-mode
  :ensure t
  :config
  (setq kotlin-tab-width 4))

(use-package groovy-mode
  :ensure t)

(use-package rustic
  :ensure t
  :config
  (setq rustic-lsp-server 'rust-analyzer)
  (setq rustic-compile-backtrace 1))

(defvar lsp-elixir--config-options (make-hash-table))

(use-package elixir-mode
  :ensure t
  :config
  (add-to-list 'exec-path "/home/chip/elixir-ls/release")
  (add-hook 'elixir-mode-hook 'lsp)
  (add-hook 'lsp-after-initialize-hook
            (lambda () (lsp--set-configuration `(:elixirLS ,lsp-elixir--config-options)))))

(use-package exunit
  :ensure t)

;; (use-package inf-elixir
;;   :load-path "packages/inf-elixir/"
;;   :bind (
;;     ("C-c C-l i i" . 'inf-elixir)
;;     ("C-c C-l i p" . 'inf-elixir-project)
;;     ("C-c C-l i l" . 'inf-elixir-send-line)
;;     ("C-c C-l i r" . 'inf-elixir-send-region)
;;     ("C-c C-l i b" . 'inf-elixir-send-buffer)))

(use-package yaml-mode
  :ensure t)

(defun chip/yaml-toggle-fold ()
  "Toggle fold all lines larger than indentation on current line"
  (interactive)
  (let ((col 1))
    (save-excursion
      (back-to-indentation)
      (setq col (+ 1 (current-column)))
      (set-selective-display
       (if selective-display nil (or col 1))))))

(use-package toml-mode
  :ensure t)

(use-package markdown-mode
  :ensure t)

(require 'ob-lilypond)

(use-package pug-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist (cons (rx ".jade" eos) 'pug-mode))
  (add-to-list 'auto-mode-alist (cons (rx ".pug" eos) 'pug-mode)))

(use-package stylus-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist (cons (rx ".styl" eos) 'pug-mode)))

(use-package glsl-mode
  :ensure t)

(require 'bolt-mode)

(defun autoremote-send (message)
  (if (boundp 'autoremote-api-key)
      (call-process-shell-command
       (format "AUTOREMOTE_API_KEY=\"%s\" autoremote %s" autoremote-api-key message))
    (message (format "No autoremote key set - unable to send message \"%s\"" message))))

(defun prose--finish ()
  (interactive)
  (clipboard-kill-ring-save (point-min) (point-max))
  (message "Prose saved to kill ring.")
  (bury-buffer))

(defun prose--clear ()
  (interactive)
  (kill-region (point-min) (point-max))
  (message "Prose cleared."))

;; (defvar prose-mode-map
;;   (let ((map (make-sparse-keymap)))
;;     ;; (set-keymap-parent map olivetti-mode-map)
;;     (define-key map "\C-c\C-c" 'prose--finish)))

(define-minor-mode prose-mode
  "Mode for writing prose."
  :global nil
  :keymap `(;; (,(kbd "C-c C-c") . prose--finish)
            ("\C-c\C-c" . prose--finish)
            ("\C-c\C-k" . prose--clear)))

(defun prose ()
  (interactive)
  (switch-to-buffer
   (get-buffer-create "*prose*"))
  ;; (setq left-margin-width 16)
  ;; (setq right-margin-width 16)
  ;; (set-window-margins (get-buffer-window) 16 16)
  (setq header-line-format (with-face " " :height 8.0))
  (olivetti-mode)
  (prose-mode)
  (set (make-local-variable 'header-line)
       'prose-header-line))

(use-package ivy-pass
  :ensure t
  :after (ivy))

(defmacro with-pass (args &rest body)
  (declare (indent 1))
  (let ((name (car args))
        (key (cadr args)))
    `(let ((,name (password-store-get ,key)))
       ,@body)))

(add-hook 'shell-mode-hook (lambda () (display-line-numbers-mode -1)))
(add-hook 'eshell-mode-hook (lambda () (display-line-numbers-mode -1)))
(setq sh-basic-offset 2)

(defun chip/header-shell ()
  (interactive)
  (setq header-line-format
        '("" ;; invocation-name
          (:eval
           (funcall header-format-filepath
                    (abbreviate-file-name default-directory))))))
(add-hook 'shell-mode-hook (lambda () (chip/header-shell)))

(use-package vterm
  :ensure t
  :config
  (setq vterm-shell "fish")
  (general-define-key
   :keymaps 'vterm-mode-map
   "<prior>" 'scroll-down-command
   "<next>" 'scroll-up-command)
  ;; line highlight flickers in vterm, so disable it
  (add-hook 'vterm-mode-hook (lambda () (setq-local global-hl-line-mode nil))))

(defun chip/vterm-toggle-cd ()
  "Toggles vterm buffer with current working directory."
  (interactive)
  (let ((dir default-directory))
    (vterm-toggle-cd)
    (vterm-toggle-insert-cd)
    (cd dir)))

(use-package vterm-toggle
  :ensure t
  :config
  (general-define-key
   "C-c t" 'vterm-toggle
   "C-c T" 'chip/vterm-toggle-cd))

(use-package multi-term
  :ensure t
  :config
  (setq multi-term-program "/bin/bash"))

(defun android-screenshot (path)
  "Take a screenshot of the currently connected Android device."
  (interactive "FPath: ")
  (shell-command (format "droidscrot %s" path)))

(defun android-screenshot-shrinked (path)
  "Take a screenshot of the currently connected Android device and resize it to fit org-reveal."
  (interactive "FPath: ")
  (android-screenshot path)
  (shell-command (format "shrink_image.sh %s" path)))

(defun android-screenshot-shrinked-with-link (path)
  "Take a shrinked screenshot and insert link."
  (interactive "FPath: ")
  (android-screenshot-shrinked path)
  (org-insert-link nil (format "file:%s" path) nil))

(use-package quelpa
  :ensure t
  :config
  (setq quelpa-checkout-melpa-p nil)    ; we're not using it for MELPA packages
  (quelpa '(tayl :repo "chip2n/tayl.el" :fetcher github))
  (quelpa '(vasttrafik :repo "chip2n/vasttrafik.el" :fetcher github)))

(defun increment-number-at-point ()
  (interactive)
  (skip-chars-backward "0-9")
  (or (looking-at "[0-9]+")
      (error "No number at point"))
  (replace-match (number-to-string (1+ (string-to-number (match-string 0))))))

(defun date->unix ()
  (interactive)
  (let ((d (org-read-date)))
    (if (region-active-p) (delete-region (region-beginning) (region-end)))
    (insert
     (string-trim-right
      (shell-command-to-string (format "timestamp %s" d))))))

(defun unix->date ()
  (interactive)
  (shell-command-to-string
   (let ((timestamp (if (region-active-p)
                        (buffer-substring (region-beginning) (region-end))
                      (read-string "Timestamp:"))))
     (message
      (string-trim-right
       (shell-command-to-string (format "unix-to-date %s" timestamp)))))))

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

(defun chip/window-80 ()
  (interactive)
  (window-resize nil (- 80 (window-width)) t))

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
