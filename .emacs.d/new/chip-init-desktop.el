;;; chip-init-desktop.el -*- lexical-binding: t -*-

;; Copyright (C) 2021  Andreas Arvidsson
;;
;; Author: Andreas Arvidsson <andreas@arvidsson.io>
;; Keywords: config
;;
;; This file is not part of GNU Emacs
;;
;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Code:

;; (require 'chip-wm)

(require 'chip-theme-desktop)
(require 'chip-keys)

(require 'chip-colors)
(chip-theme-set-dark)
(require 'chip-faces)
(chip-faces)

(require 'chip-window)
(require 'chip-modeline)
(require 'chip-headerline)
;; (require 'chip-agenda)

(require 'chip-doc)
(require 'chip-vc)
(require 'chip-completion)
(require 'chip-code)
(require 'chip-code-lisp)
(require 'chip-lang)

;;; Package: paren-face

;; I'm using this package to make individual faces for each parentheses. This
;; allows us to dim them in the theme. Makes lispy code way more readable, yay!

(use-package paren-face
  :config
  (global-paren-face-mode)
  (setq paren-face-regexp "[][(){}]"))

;;; Package: all-the-icons

;; Using fancy icons in some places (e.g. ~treemacs~) to spice things up. This
;; package includes icons from a bunch of different sources.

(use-package all-the-icons)

(use-package all-the-icons-dired
  :after (all-the-icons)
  :config
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))

;;; Fringes

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

;;; Security

(require 'private)
(setq auth-sources '("~/.authinfo.gpg"))

(use-package pass)

(defmacro with-pass (args &rest body)
  (declare (indent 1))
  (let ((name (car args))
        (key (cadr args)))
    `(let ((,name (password-store-get ,key)))
       ,@body)))

;;; General

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
(setq browse-url-generic-program "firefox")
(setq browse-url-browser-function 'browse-url-generic)

(setq select-enable-clipboard t)

(add-hook 'image-mode-hook 'auto-revert-mode)

;; skip confirmation when killing processes
(setq kill-buffer-query-functions
      (delq 'process-kill-buffer-query-function kill-buffer-query-functions))

;;; Text navigation

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

(after-load (general)
  (general-define-key
   :states '(normal insert visual emacs)
   "C-e"   'chip/move-end-of-line
   "C-a"   'chip/move-beginning-of-line
   "C-c i" 'imenu
   "C-s"   'avy-goto-char-2
   "C-c s" 'avy-goto-char-2
   "M-s"   'consult-line)

  (general-define-key
   :keymaps '(flymake-mode-map)
   "M-n" 'flymake-goto-next-error
   "M-p" 'flymake-goto-prev-error)

  (general-define-key
   :keymaps '(flycheck-mode-map)
   "M-n" 'flycheck-next-error
   "M-p" 'flycheck-previous-error))

(use-package evil-snipe
  :after (evil)
  :config
  (c/diminish evil-snipe-mode)
  (c/diminish evil-snipe-local-mode)

  (setq evil-snipe-scope 'buffer)
  (evil-snipe-mode +1)
  (evil-snipe-override-mode +1)
  ;; turn off evil-snipe in magit
  (add-hook 'magit-mode-hook 'turn-off-evil-snipe-override-mode))

;;; File navigation

(defun chip/open-config-file ()
  "Open Emacs configuration file."
  (interactive)
  (find-file (concat chip-config-dir "init.el")))

(use-package projectile
  :config
  (c/diminish projectile-mode)

  (add-to-list 'projectile-globally-ignored-directories "*node_modules")
  (setq projectile-enable-caching nil)
  ;; we remove -o flag so that untracked files are not included
  ;; this is mainly so that they don't always appear as the first search
  (setq projectile-git-command "git ls-files -zc --exclude-standard")
  (setq projectile-indexing-method 'alien)
  (projectile-register-project-type 'shadow-cljs '("shadow-cljs.edn")
                                    :src-dir "src/main"
                                    :test-dir "src/test"
                                    :test-suffix "_test")
  (projectile-mode))

;; (use-package counsel-projectile
;;   :after (counsel projectile))

;;; Package: Treemacs

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

;; My treemacs theme is based on the Doom Treemacs theme (https://github.com/hlissner/emacs-doom-themes),
;; but customized to fit the rest of my configuration.

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

(use-package treemacs
  :after (evil)
  :config
  (setq treemacs-show-cursor nil)
  (setq treemacs-indentation 1)
  (setq treemacs-space-between-root-nodes nil)
  (add-hook 'treemacs-mode-hook
            (lambda () (setq tab-width 1)))
  (chip/treemacs-setup-theme)
  (chip/treemacs-setup-keys))


;; I use treemacs-evil, because without it the cursor inside the treemacs
;; buffer is still visible despite setting treemacs-show-cursor to nil.

(use-package treemacs-evil
  :after (evil))


;;; Dired

(add-hook 'dired-mode-hook 'auto-revert-mode)

;; load dired-x immediately to make keybindings available
(require 'dired-x)

;; show directories before files, and dotfiles first
(setq dired-listing-switches "-aBhlv --group-directories-first")

;; start dired in emacs mode
(after-load (evil)
  (add-to-list 'evil-emacs-state-modes 'dired-mode))

;;; Package: deadgrep

(use-package deadgrep
  :after (evil)
  :config
  (add-to-list 'evil-emacs-state-modes 'deadgrep-mode))

;;; Package: rg

(use-package rg)

;;; Package: wgrep

(use-package wgrep)

;;; Buffer

(use-package golden-ratio-scroll-screen
  :config
  (setq golden-ratio-scroll-highlight-flag nil))

;; highlight TODOs in comments
(use-package hl-todo
  :hook ((prog-mode . hl-todo-mode))
  :config
  (setq hl-todo-keyword-faces
        '(("TODO" . "#ffa398")
          ("FIXME" . "#ffa398")
          ("NOTE" . "#fbf2bf")
          ("OPTIMIZE" . "#fbf2bf")
          ("HACK" . "#fbf2bf"))))

(use-package outshine
  :config
  (c/diminish outshine-mode)
  (general-define-key
   :keymaps '(outshine-mode-map)
   :states '(normal)
   "TAB" 'outshine-cycle
   "<backtab>" 'outshine-cycle-buffer)
  (setq outshine-startup-folded-p nil)
  ;; TODO make generic hook for all lisps (chip-lisp-mode-hook)
  (add-hook 'emacs-lisp-mode-hook 'outshine-mode)
  (add-hook 'lisp-mode-hook 'outshine-mode))

;;; Windows

(use-package zoom
  :config
  (setq zoom-size '(0.618 . 0.618)))

(defun chip/window-zoom ()
  (interactive)
  (zoom))

(defun chip/window-unzoom ()
  (interactive)
  (other-window 1)
  (unwind-protect
      (chip/window-zoom)
    (other-window 1)))

(after-load (general)
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
  :config
  (winner-mode 1))

;; Allows you to transpose frames (mainly via ace-window)
(require 'transpose-frame)

(use-package avy)

(use-package ace-window
  :after (ivy)
  :config
  (c/diminish ace-window-mode)
  (setq aw-dispatch-always t)
  (after-load (ivy counsel projectile)
    (ivy-add-actions
     'ivy-switch-buffer
     '(("a" ace-window "ace-window")))
    (ivy-add-actions
     'counsel-find-file
     '(("a" ace-window "ace-window")))
    (ivy-add-actions
     'counsel-projectile-find-file
     '(("a" ace-window "ace-window")))))

;;; Package: which-key

(use-package which-key
  :config
  (c/diminish which-key-mode)
  (which-key-mode))

;;; Package: sudo-edit

;; I use the sudo-edit package to allow me to enter sudo while viewing a (read-only)
;; file. This is way more convenient than the standard method of C-x C-f with a sudo: prefix.

(use-package sudo-edit)

;;; Refactoring

(defun chip/lambda->fun ()
  (interactive)
  (save-excursion
    (if (looking-at "(lambda")
        (message "done")
      (cl-loop
       until (looking-at "(lambda")
       do (backward-up-list 1 t t))
      (let* ((sexp (sexp-at-point))
             (args (cadr sexp))
             (body (caddr sexp))
             (name (read-from-minibuffer "Name: ")))
        (save-excursion
          (down-list)
          (forward-sexp 2)
          (mark-sexp)
          (kill-region (region-beginning) (region-end))

          (end-of-defun)
          (insert (format "\n\n(defun %s %s" name args))
          (yank)
          (insert ")\n")
          (beginning-of-defun)
          (chip/indent-defun))
        (kill-sexp)
        (insert (format "#'%s" name))))))

(defun chip/indent-defun ()
  "Indent the current defun."
  (interactive)
  (save-excursion
    (mark-defun)
    (indent-region (region-beginning) (region-end))))

;; Toggle between CamelCase, snake_case etc
(use-package string-inflection)

(use-package aggressive-indent)

(use-package yasnippet
  :init
  :config
  (yas-global-mode 1)
  (add-to-list 'yas-snippet-dirs (locate-user-emacs-file "snippets")))

(setq compilation-scroll-output t)
(after-load (general)
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
  :config
  (c/diminish company-mode)

  (general-define-key
   :keymap 'prog-mode-map
   "M-/" 'counsel-company)
  (add-hook 'company-mode-hook 'chip/company-setup-keys)
  ;; prevent downcasing when autocompleting
  (setq company-dabbrev-downcase nil)
  (setq evil-complete-next-func 'complete-complete-cycle-next)
  (setq evil-complete-previous-func 'complete-complete-cycle-previous)

  ;; show company completion with delay
  (setq company-idle-delay 0.3)

  ;; show suggestions after entering one character.
  (setq company-minimum-prefix-length 1)

  (setq company-selection-wrap-around t))

(defun complete-complete-cycle-next (arg)
  (company-complete-common-or-cycle))

(defun complete-complete-cycle-previous (arg)
  (company-complete-common-or-cycle -1))

(use-package company-box
  :after (company)
  :hook (company-mode . company-box-mode)
  :config
  (c/diminish company-box-mode))

;; communication with language servers generate a lot of garbage
(setq gc-cons-threshold 100000000)

;; show message when garbage collection happens
(setq-default garbage-collection-messages t)

;; language servers often generate large responses
(setq read-process-output-max (* 1024 1024))

;;; Package: lsp-mode

(use-package lsp-mode
  :config
  (setq lsp-prefer-flymake nil)
  (setq lsp-eldoc-enable-hover nil)
  (setq lsp-enable-xref nil)
  (setq lsp-ui-sideline-enable nil)
  (setq lsp-ui-doc-enable nil)
  (setq lsp-auto-guess-root t)
  (setq lsp-headerline-breadcrumb-enable nil)

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

(use-package evil-nerd-commenter
  :bind (:map prog-mode-map
              (("C-;" . evilnc-comment-or-uncomment-lines))))

(use-package evil
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
  (add-to-list 'evil-emacs-state-modes 'image-mode)
  (add-hook 'help-mode-hook 'evil-emacs-state))

(use-package evil-visualstar
  :after (evil)
  :config
  (global-evil-visualstar-mode))

(use-package evil-collection
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

;;; org-mode

(require 'chip-org)

(require 'org-protocol)

(use-package org-roam
  :hook
  (after-init . org-roam-mode)
  :bind (:map org-roam-mode-map
         (("C-c n l" . org-roam)
          ("C-c n t" . org-roam-dailies-find-today)
          ("C-c n f" . org-roam-find-file)
          ("C-c n c" . org-roam-dailies-capture-today))
         :map org-mode-map
         (("C-c n i" . org-roam-insert)))
  :config
  (c/diminish org-roam-mode)

  (setq org-roam-directory "/home/chip/org/personal/roam")
  (setq org-roam-dailies-directory "daily/")
  (setq org-roam-buffer-position 'left)
  (setq org-roam-buffer-width 0.2)
  (setq org-roam-encrypt-files nil)
  (setq org-roam-db-location "/home/chip/.org-roam.db")

  (setq org-roam-dailies-capture-templates
      '(("e" "entry" entry
         #'org-roam-capture--get-point
         "* %?\n%T"
         :file-name "daily/%<%Y-%m-%d>"
         :head "#+title: %<%Y-%m-%d>\n")
        ("u" "supplements" entry
         #'org-roam-capture--get-point
         "* Supplements\n%T\n| %? |  |"
         :file-name "daily/%<%Y-%m-%d>"
         :head "#+title: %<%Y-%m-%d>\n")
        ("s" "summary" entry
         #'org-roam-capture--get-point
         "* Day summary\n%T\n%?\n\n%(org-clock-report-today)"
         :file-name "daily/%<%Y-%m-%d>"
         :head "#+title: %<%Y-%m-%d>\n")


        ("w" "Workout")
        ("wa" "Workout A" entry
         #'org-roam-capture--get-point
         "
* Workout
%T
| Bulgarian Split Squat    | 3x10 | %?  |
| Bench Press              | 3x10 |   |
| Straight-Legged Deadlift | 3x10 |   |
| Plank                    | 3x10 | - |
"
         :file-name "daily/%<%Y-%m-%d>"
         :head "#+title: %<%Y-%m-%d>\n"
         :clock-in t
         :clock-resume t)
        ("wb" "Workout B" entry
         #'org-roam-capture--get-point
         "
* Workout
%T
| Bulgarian Split Squat | 3x10 | %?  |
| Seated Shoulder Press | 3x10 |   |
| Bent Over Row         | 3x10 |   |
| Plank                 | 3x10 | - |
"
         :file-name "daily/%<%Y-%m-%d>"
         :head "#+title: %<%Y-%m-%d>\n"
         :clock-in t
         :clock-resume t)))

  (add-to-list 'evil-emacs-state-modes 'org-roam-backlinks-mode)
  (require 'org-roam-protocol))

(use-package org-roam-server)

(use-package deft
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

;;; Apps

(defun youtube-rss (cid)
  (format "https://www.youtube.com/feeds/videos.xml?channel_id=%s" cid))

(defun reddit-rss (feed user)
  (format "https://www.reddit.com/.rss?feed=%s&user=%s" feed user))

(use-package elfeed
  :config
  (setq shr-inhibit-images t)           ; disable image loading when viewing entries
  (setq elfeed-feeds
        `(("http://feeds.feedburner.com/blogspot/hsDu" android) ; Android Developers Blog
          ("http://oremacs.com/atom.xml" emacs) ; (or emacs)
          ("http://pragmaticemacs.com/feed/" emacs) ; Pragmatic Emacs
          ("https://emacsair.me/feed.xml" emacs)
          ("https://protesilaos.com/codelog.xml" emacs) ; Protesilaos Stavrou blog
          ("https://defn.io/index.xml" racket)
          "http://techsnuffle.com/feed.xml"
          "https://ultimatemachine.se/articles.xml"
          "https://hnrss.org/newest?comments=10"
          ,(youtube-rss "UCaxar6TBM-94_ezoS00fLkA") ; Day9TV
          ,(youtube-rss "UC0uTPqBCFIpZxlz_Lv1tk_g") ; Protesilaos Stavrou
          ,(reddit-rss private/reddit-rss-feed "chip2n")
          ))

  (add-to-list 'evil-emacs-state-modes 'elfeed-search-mode)
  (add-to-list 'evil-emacs-state-modes 'elfeed-show-mode)

  ;; (general-define-key
  ;;  :states 'normal
  ;;  :keymaps '(elfeed-search-mode-map)
  ;;  "f" 'elfeed-search-set-filter
  ;;  "r" 'elfeed-update
  ;;  "o" 'elfeed-search-show-entry
  ;;  "q" 'elfeed-kill-buffer)

  ;; (general-define-key
  ;;  :states 'normal
  ;;  :keymaps '(elfeed-show-mode-map)
  ;;  "q" 'elfeed-kill-buffer
  ;;  "n" 'elfeed-show-next
  ;;  "p" 'elfeed-show-prev)
  )

(use-package emms
  :config
  (emms-all)
  (emms-default-players))

(use-package slack
  :commands (slack-start)
  :init
  (setq slack-buffer-emojify t)
  (setq slack-prefer-current-team t)
  :config
  (slack-register-team
   :name "remente"
   :default t
   :client-id private/slack-client-id-remente
   :client-secret private/slack-client-secret-remente
   :token private/slack-token-remente))

(use-package alert
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
  :config
  (pdf-tools-install)
  (add-hook 'pdf-view-mode-hook (lambda () (blink-cursor-mode -1))))

(use-package pomidor
  :config
  (setq pomidor-sound-tick nil
        pomidor-sound-tack nil
        ;; pomidor-sound-overwork (expand-file-name (concat pomidor-dir "overwork.wav"))
	;; pomidor-sound-break-over (expand-file-name (concat (getenv "HOME") "/Music/overwork.wav"))
        ))

;;; Languages

;; Toggle evil emacs state when entering edebug mode
(add-hook 'edebug-mode-hook
          (lambda ()
            (if (bound-and-true-p edebug-mode)
                (evil-emacs-state)
              (evil-normal-state))))

(after-load (company)
  (add-hook 'emacs-lisp-mode-hook 'company-mode))

(use-package geiser
  :after (evil)
  :config
  (setq geiser-chicken-binary "chicken-csi")
  (setq geiser-active-implementations '(chicken racket))
  (setq geiser-default-implementation 'chicken)
  (add-hook 'geiser-repl-mode-hook 'evil-lispy-mode)
  (add-to-list 'evil-emacs-state-modes 'geiser-debug-mode))

(use-package racket-mode
  :config
  (add-to-list 'auto-mode-alist (cons (rx ".rkt" eos) 'racket-mode)))

;; In order to make Forth play nicely with org-babel, you need the gforth compiler
;; as well as forth-mode distributed in with gforth in gforth.el. I've copied this
;; file from the 0.7.3 because I'm a lazy boy, so I'll just require it.
(require 'forth-mode)

(use-package clojure-mode)

(use-package cider
  :config
  (setq cider-test-show-report-on-success nil)
  (setq cider-auto-select-test-report-buffer nil)
  (eldoc-mode t)
  (add-to-list 'evil-motion-state-modes 'cider-test-report-mode)
  (add-hook 'cider-mode-hook 'company-mode)
  (add-hook 'cider-repl-mode-hook 'company-mode))

(use-package inf-clojure)

(use-package clj-refactor)

(use-package anaconda-mode
  :config
  (add-hook 'python-mode-hook 'anaconda-mode))

(use-package pyvenv)

(use-package company-anaconda
  :after (company anaconda-mode)
  :config
  (add-to-list 'company-backends 'company-anaconda))

(use-package dart-mode
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
  :after (lsp-mode lsp-ui)
  :config
  (setq lsp-haskell-process-path-hie "hie-wrapper")
  (add-hook 'haskell-mode-hook 'lsp-haskell-enable)
  (add-hook 'haskell-mode-hook 'flycheck-mode))

;; nvm

(defun chip/nvm-10.23.0 ()
  (interactive)
  (nvm-use "v10.23.0"))

(defun chip/vterm-nvm-10.23.0 ()
  (interactive)
  (nvm-use "v10.23.0" (lambda () (vterm "*vterm-nvm-10.23.0*"))))

(defun chip/nvm-12.10.0 ()
  (interactive)
  (nvm-use "v12.10.0"))

(use-package nvm
  :config
  (chip/nvm-12.10.0))

;; js2-mode

(use-package js2-mode
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
  :config
  ;; add simple validation through flycheck (for missing commas etc)
  (add-hook 'json-mode-hook #'flycheck-mode))

(use-package typescript-mode)

(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  (company-mode +1))

(use-package tide
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
  :after (company)
  :config
  (add-to-list 'company-backends 'company-omnisharp)
  (add-hook 'csharp-mode-hook #'company-mode)
  (add-hook 'csharp-mode-hook 'omnisharp-mode))

(use-package kotlin-mode
  :config
  (setq kotlin-tab-width 4))

(use-package groovy-mode)

(use-package rustic
  :config
  (setq rustic-lsp-server 'rust-analyzer)
  (setq rustic-compile-backtrace 1)
  (add-hook 'rustic-mode-hook
            (lambda () (setq company-backends '(company-capf)))))

(defvar lsp-elixir--config-options (make-hash-table))

(use-package elixir-mode
  :config
  (add-to-list 'exec-path "/home/chip/elixir-ls/release")
  (add-hook 'elixir-mode-hook 'lsp)
  (add-hook 'lsp-after-initialize-hook
            (lambda () (lsp--set-configuration `(:elixirLS ,lsp-elixir--config-options)))))

(use-package exunit)

;; (use-package inf-elixir
;;   :load-path "packages/inf-elixir/"
;;   :bind (
;;     ("C-c C-l i i" . 'inf-elixir)
;;     ("C-c C-l i p" . 'inf-elixir-project)
;;     ("C-c C-l i l" . 'inf-elixir-send-line)
;;     ("C-c C-l i r" . 'inf-elixir-send-region)
;;     ("C-c C-l i b" . 'inf-elixir-send-buffer)))

(use-package yaml-mode)

(defun chip/yaml-toggle-fold ()
  "Toggle fold all lines larger than indentation on current line"
  (interactive)
  (let ((col 1))
    (save-excursion
      (back-to-indentation)
      (setq col (+ 1 (current-column)))
      (set-selective-display
       (if selective-display nil (or col 1))))))

(use-package toml-mode)

(use-package markdown-mode)

(require 'ob-lilypond)

(use-package glsl-mode)

(require 'bolt-mode)

(use-package zig-mode
  :after (lsp-mode)
  :bind (:map zig-mode-map
              ("C-c C-r" . chip/zig-compile-run)
              ("C-c C-b" . chip/zig-compile)
              ("C-c C-t" . chip/zig-test)
              ("C-c C-f" . lsp-format-buffer))
  :config
  ;; formatting on save breaks lsp-mode
  ;; see https://github.com/ziglang/zig-mode/issues/49
  (setq zig-format-on-save nil)
  (add-to-list 'lsp-language-id-configuration '(zig-mode . "zig"))
  (add-hook 'zig-mode-hook 'lsp)
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-stdio-connection "/home/chip/Downloads/x86_64-linux/zls")
    :major-modes '(zig-mode)
    :server-id 'zls)))

(defun chip/zig-compile ()
  "Compile using `zig build`."
  (interactive)
  (let ((default-directory (projectile-project-root)))
    (zig--run-cmd "build")))

(defun chip/zig-compile-run ()
  "Compile and run using `zig build run`."
  (interactive)
  (let ((default-directory (projectile-project-root)))
    (zig--run-cmd "build run")))

(defun chip/zig-test ()
  "Test using `zig build test`."
  (interactive)
  (let ((default-directory (projectile-project-root)))
    (zig--run-cmd "build test")))

(use-package gdscript-mode
  :config
  ;; suppress unknown notification errors.
  (add-hook 'gdscript-mode-hook 'lsp)
  (advice-add #'lsp--get-message-type :around #'lsp--gdscript-ignore-errors))

(defun lsp--gdscript-ignore-errors (original-function &rest args)
  "Ignore the error message resulting from Godot not replying to the `JSONRPC' request."
  (if (string-equal major-mode "gdscript-mode")
      (let ((json-data (nth 0 args)))
        (if (and (string= (gethash "jsonrpc" json-data "") "2.0")
                 (not (gethash "id" json-data nil))
                 (not (gethash "method" json-data nil)))
            nil ; (message "Method not found")
          (apply original-function args)))
    (apply original-function args)))

;;; Autoremote

(defun autoremote-send (message)
  (if (boundp 'autoremote-api-key)
      (call-process-shell-command
       (format "AUTOREMOTE_API_KEY=\"%s\" autoremote %s" autoremote-api-key message))
    (message (format "No autoremote key set - unable to send message \"%s\"" message))))

;;; Olivetti

(use-package olivetti
  :config
  (c/diminish olivetti-mode))

;;; Prose

(require 'chip-prose)

;;; Shell

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
  :config
  (general-define-key
   "C-c t" 'vterm-toggle
   "C-c T" 'chip/vterm-toggle-cd))

(use-package multi-term
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

;;; Utils

(defun increment-number-at-point ()
  (interactive)
  (skip-chars-backward "0-9")
  (or (looking-at "[0-9]+")
      (error "No number at point"))
  (replace-match (number-to-string (1+ (string-to-number (match-string 0))))))

(defun decrement-number-at-point ()
  (interactive)
  (skip-chars-backward "0-9")
  (or (looking-at "[0-9]+")
      (error "No number at point"))
  (replace-match (number-to-string (1- (string-to-number (match-string 0))))))

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

(provide 'chip-init-desktop)

;;; chip-init-desktop.el ends here
