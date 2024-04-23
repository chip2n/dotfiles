;;; chip-init-desktop.el -*- lexical-binding: t -*-

;; Copyright (C) 2022  Andreas Arvidsson
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

(when c/mac? (require 'chip-init-mac))

;; (require 'chip-wm)

(require 'private)
(require 'chip-theme-desktop)
(require 'chip-keys)

(require 'chip-colors)
(chip-theme-set-dark)
(require 'chip-faces)
(chip-faces)

(require 'chip-modeline)
(require 'chip-headerline)

(when c/config-evil?
  (require 'chip-evil))
(when c/config-meow?
  (require 'chip-meow))

(require 'chip-window)
(require 'chip-registers)
(require 'chip-bookmark)
(require 'chip-project)

(require 'chip-doc)
(require 'chip-watch)
(require 'chip-export)
(require 'chip-vc)
(require 'chip-completion)
(require 'chip-code)
(require 'chip-code-dbg)

(require 'chip-code-lisp)
(require 'chip-code-c)
;; (require 'chip-code-elixir)
(require 'chip-code-elixir2)
(require 'chip-code-zig)
(require 'chip-code-nim)
(require 'chip-code-sql)
(require 'chip-lang)
;(require 'chip-present)
(require 'chip-email)
(require 'chip-gpt)

;;; Package: paren-face

;; I'm using this package to make individual faces for each parentheses. This
;; allows us to dim them in the theme. Makes lispy code way more readable, yay!

;; (use-package paren-face
;;   :config
;;   (global-paren-face-mode)
;;   (setq paren-face-regexp "[][(){}]"))

;;; Security

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

;; disable progressive speed when scrolling
(setq mouse-wheel-progressive-speed nil)

;; make mouse scrolling faster
(setq mouse-wheel-scroll-amount
      '(4
        ((shift)
         . hscroll)
        ((meta))
        ((control meta)
         . global-text-scale)
        ((control)
         . text-scale)))

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

;; don't ring bell on error
(setq ring-bell-function 'ignore)

;; remove keybinding for suspend-frame, I keep hitting it accidentally because I'm a troglodyte
(global-unset-key (kbd "C-z"))

(use-package undo-tree)

;;; direnv

(use-package direnv
  :config
  ;; It's always OK to run `direnv-update-environment' automatically - the tool
  ;; itself handles the security aspect.
  (add-to-list 'safe-local-variable-values '(eval direnv-update-environment)))

;;; Help

(use-package helpful
  :bind (("C-h k" . helpful-key)
         ("C-h v" . helpful-variable)
         ("C-h f" . helpful-function)
         ("C-h ." . helpful-at-point)))

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
   "C-e"   'chip/move-end-of-line
   ;; "C-a"   'chip/move-beginning-of-line
   "C-c i" 'imenu
   "C-s"   'isearch-forward
   "C-r"   'isearch-backward
   "C-c s" 'avy-goto-char-timer
   "M-s"   'consult-line
   "C-S-s" 'consult-line))

(defun c/isearch-kill-result ()
  "Kill result of text marked by isearch."
  (interactive)
  (if (use-region-p)
      (call-interactively 'kill-region)
    (kill-region (point) isearch-other-end))
  (isearch-exit))

(defun c/isearch-copy-result ()
  "Copy result of text marked by isearch."
  (interactive)
  (copy-region-as-kill isearch-other-end (point))
  (isearch-exit))

(defun c/isearch-done-beginning (&optional nopush edit)
  "End current search at the start of the match.
The default is to leave the cursor where it is, which is not as useful when searching forward."
  (interactive)
  (funcall #'isearch-done nopush edit)
  (when (and isearch-forward isearch-other-end)
    (goto-char isearch-other-end)))

;; (define-key isearch-mode-map (kbd "<return>") 'c/isearch-done-beginning)
(define-key isearch-mode-map (kbd "C-<return>") 'c/isearch-done-beginning)
(define-key isearch-mode-map (kbd "C-S-w") 'c/isearch-kill-result)
(define-key isearch-mode-map (kbd "M-w") 'c/isearch-copy-result)

;; Enable simple fuzzy matching in isearch
(setq search-whitespace-regexp ".*?")

(use-package iedit)

(use-package evil-snipe
  :after (evil)
  :config
  (c/diminish evil-snipe-mode)
  (c/diminish evil-snipe-local-mode)

  (setq evil-snipe-scope 'buffer)
  (evil-snipe-mode +1)
  (evil-snipe-override-mode +1)

  ;; I don't want search to repeat when tapping s/f again (I use ; for this)
  (setq evil-snipe-repeat-keys nil)

  ;; turn off evil-snipe in magit
  (add-hook 'magit-mode-hook 'turn-off-evil-snipe-override-mode))

;;; Regions

(use-package expand-region
  :bind (("C-c m" . er/expand-region)))

;; Allow C-w, M-w etc operate on current line if no region active
(use-package whole-line-or-region
  :config
  (whole-line-or-region-global-mode))

;;; File navigation

(defun chip/open-config-file ()
  "Open Emacs configuration file."
  (interactive)
  (find-file (concat chip-config-dir "init.el")))

;;; Package: Treemacs

(defun chip/treemacs-setup-keys ()
  (general-define-key
   :states '(normal)
   "<backspace>" 'treemacs
   "S-<backspace>" 'treemacs-select-window
   "M-o" 'ace-window)

  (general-define-key
   :keymaps '(treemacs-mode-map)
   "<next>" 'scroll-up-command
   "<prior>" 'scroll-down-command
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
                                           :face face)))
      (let ((root-icon (icon "repo" :v-adjust -0.1 :face root-face))
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
            (lock-icon (icon "lock"))
            (settings-icon (icon "gear"))
            (git-icon (icon "git-branch"))
            (parent-closed-icon "+")
            (parent-opened-icon "-")
            (cmake-icon (icon "tools")))
        (treemacs-create-theme "chip"
          :icon-directory "~/.emacs.d/icons"
          :config
          (progn
            (treemacs-create-icon :icon (format " %s\t" root-icon) :extensions (root-open))
            (treemacs-create-icon :icon (format " %s\t" root-icon) :extensions (root-closed))
            (treemacs-create-icon :icon (format "%s\t%s " parent-opened-icon dir-icon) :extensions (dir-open))
            (treemacs-create-icon :icon (format "%s\t%s " parent-closed-icon dir-icon) :extensions (dir-closed))
            (treemacs-create-icon :icon (format "%s\t%s " parent-opened-icon pkg-icon) :extensions (tag-open))
            (treemacs-create-icon :icon (format "%s\t%s " parent-closed-icon pkg-icon) :extensions (tag-closed))
            (treemacs-create-icon :icon (format "%s\t%s " parent-closed-icon dir-icon) :extensions ("src-closed"))
            (treemacs-create-icon :icon (format "%s\t%s " parent-opened-icon dir-icon) :extensions ("src-open"))
            (treemacs-create-icon :icon (format "%s\t%s " parent-opened-icon dir-icon) :extensions ("build-open"))
            (treemacs-create-icon :icon (format "%s\t%s " parent-closed-icon dir-icon) :extensions ("build-closed"))
            (treemacs-create-icon :icon (format "%s\t%s " parent-closed-icon dir-icon) :extensions ("test-closed"))
            (treemacs-create-icon :icon (format "%s\t%s " parent-opened-icon dir-icon) :extensions ("test-open"))
            (treemacs-create-icon :icon (format "\t\t%s " tag-icon) :extensions (tag-leaf))
            (treemacs-create-icon :icon (format "%s " error-icon) :extensions (error))
            (treemacs-create-icon :icon (format "%s " warning-icon) :extensions (warning))
            (treemacs-create-icon :icon (format "%s " info-icon) :extensions (info))
            (treemacs-create-icon
             :icon (format "  %s " media-icon)
             :extensions ("png" "jpg" "jpeg" "gif" "ico" "tif" "tiff" "svg" "bmp"
                          "psd" "ai" "eps" "indd" "mov" "avi" "mp4" "webm" "mkv"
                          "wav" "mp3" "ogg" "midi"))
            (treemacs-create-icon
             :icon (format "  %s " code-icon)
             :extensions ("adoc" "asciidoc" "bashrc" "bat" "c" "cabal" "cabal" "cask" "cc"
                          "clj" "cljc" "cljs" "cpp" "css" "csv" "cxx" "dart"
                          "dockerfile" "dockerfile" "editorconfig" "eex" "el"
                          "elm" "ex" "exs" "fish" "gradle" "gitconfig" "gitignore" "go" "h"
                          "heex" "hh" "hpp" "hs" "htm" "html" "hy" "ideavimrc" "inputrc"
                          "j2" "j2" "java" "jinja2" "jinja2" "jl" "js" "json" "jsx"
                          "kt" "kt" "kts" "lhs" "lisp" "lua" "lua" "makefile" "ml"
                          "mli" "nim" "nim" "nims" "nix" "perl" "pl" "plt" "pm"
                          "pm6" "pp" "pp" "py" "pyc" "r" "racket" "rb" "re" "rei"
                          "rkt" "rktd" "rktl" "rs" "sbt" "scala" "scm" "scrbl"
                          "scribble" "scss" "sh" "sql" "sqlite" "sql" "styles" "sv"
                          "tex" "toml" "tpp" "tridactylrc" "ts" "tsx" "v" "vagrantfile"
                          "vagrantfile" "vh" "vimperatorrc" "vimrc" "vrapperrc"
                          "vue" "xml" "xsl" "yaml" "yml" "zig" "zsh" "zshrc" "edn"))
            (treemacs-create-icon
             :icon (format "  %s " book-icon)
             :extensions ("lrf" "lrx" "cbr" "cbz" "cb7" "cbt" "cba" "chm" "djvu"
                          "doc" "docx" "pdb" "pdb" "fb2" "xeb" "ceb" "inf" "azw"
                          "azw3" "kf8" "kfx" "lit" "prc" "mobi" "exe" "or" "html"
                          "pkg" "opf" "txt" "pdb" "ps" "rtf" "pdg" "xml" "tr2"
                          "tr3" "oxps" "xps"))
            (treemacs-create-icon
             :icon (format "  %s " text-icon)
             :extensions ("md" "markdown" "rst" "log" "org" "txt"
                          "CONTRIBUTE" "LICENSE" "README" "CHANGELOG"))
            (treemacs-create-icon
             :icon (format "  %s " binary-icon)
             :extensions ("exe" "dll" "obj" "so" "o" "out" "elc"))
            (treemacs-create-icon
             :icon (format "  %s " lock-icon)
             :extensions ("lock"))
            (treemacs-create-icon
             :icon (format "  %s " settings-icon)
             :extensions ("cfg" "properties" "ini"))
            (treemacs-create-icon
             :icon (format "  %s " pdf-icon)
             :extensions ("pdf"))
            (treemacs-create-icon
             :icon (format "  %s " zip-icon)
             :extensions ("zip" "7z" "tar" "gz" "rar" "tgz"))
            (treemacs-create-icon
             :icon (format "  %s " text-icon)
             :extensions ("package.json" "package-lock.json"))
            (treemacs-create-icon
             :icon (format "  %s " text-icon)
             :extensions (fallback))
            (treemacs-create-icon
             :icon (format "  %s " git-icon)
             :extensions ("git" "gitignore" "gitconfig" "gitmodules" "gitattributes"))
            (treemacs-create-icon
             :icon (format "  %s " cmake-icon)
             :extensions ("cmake" "CMakeLists.txt")))))))
  (treemacs-load-theme "chip"))

(use-package treemacs
  :defer t
  :bind ("C-c F" . treemacs-select-window)
  :config
  (setf treemacs-window-background-color (cons "#1b1e24f" "#21242b"))
  (setq treemacs-show-cursor nil)
  (setq treemacs-indentation 1)
  (setq treemacs-space-between-root-nodes nil)
  (setq treemacs-is-never-other-window t)
  (add-hook 'treemacs-mode-hook
            (lambda () (setq tab-width 1)))
  (chip/treemacs-setup-theme)
  (chip/treemacs-setup-keys))

;; I use treemacs-evil, because without it the cursor inside the treemacs
;; buffer is still visible despite setting treemacs-show-cursor to nil.

(use-package treemacs-evil
  :defer t
  :after (evil))

;;; Dired

(defun c/dired-setup ()
  (auto-revert-mode)
  (dired-hide-details-mode))

(add-hook 'dired-mode-hook 'c/dired-setup)

;; load dired-x immediately to make keybindings available
(require 'dired-x)

;; show directories before files, and dotfiles first
(setq dired-listing-switches "-aBhlv --group-directories-first")

;; no confirm when copying directories recursively
(setq dired-recursive-copies 'always)

;; if two dired windows visible, autocomplete the other buffers path
(setq dired-dwim-target 1)

;; start dired in emacs mode
(after-load (evil)
  (add-to-list 'evil-emacs-state-modes 'dired-mode))

;; Stolen from: https://emacs.stackexchange.com/a/51614/31683
(define-minor-mode dired-follow-mode
  "Diplay file at point in dired after a move."
  :lighter "dired-follow"
  :global t
  (if dired-follow-mode
      (advice-add 'dired-next-line :after (lambda (arg) (dired-display-file)))
    (advice-remove 'dired-next-line (lambda (arg) (dired-display-file)))))

(use-package dired-narrow)

;;; Package: deadgrep

(use-package deadgrep
  :config
  (after-load (evil)
    (add-to-list 'evil-emacs-state-modes 'deadgrep-mode)))

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
          ("NOCOMMIT" . "#ffa398")
          ("NOTE" . "#fbf2bf")
          ("OPTIMIZE" . "#fbf2bf")
          ("HACK" . "#fbf2bf"))))

;;; Comint

;; I always want comint to scroll to the bottom on output by default so that the
;; prompt is always visible
(setq comint-scroll-to-bottom-on-output t)

;; TODO These doesn't seem to work?
(general-define-key
 :keymaps '(comint-mode-map)
  "M-p" 'comint-previous-input
  "M-n" 'comint-next-input)

;;; Package: which-key

(use-package which-key
  :config
  (c/diminish which-key-mode)
  (which-key-mode))

;;; Package: sudo-edit

;; I use the sudo-edit package to allow me to enter sudo while viewing a (read-only)
;; file. This is way more convenient than the standard method of C-x C-f with a sudo: prefix.

(use-package sudo-edit
  :defer t)

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
  :defer t
  :config
  (c/diminish yas-minor-mode)
  (yas-global-mode 1)
  (add-to-list 'yas-snippet-dirs (locate-user-emacs-file "snippets")))

;; language servers often generate large responses
(setq read-process-output-max (* 1024 1024))

;;; Package: lsp-mode

(defun c/lsp-avy-lens ()
  "Calls `lsp-avy-lens' and refreshes all lenses afterwards."
  (interactive)
  (lsp-avy-lens)
  (lsp-lens-refresh t))

(defun c/lsp--refresh-imenu (x)
  "Refresh lsp-ui-imenu if active."
  (when (and (bound-and-true-p lsp-mode)
             (get-buffer lsp-ui-imenu-buffer-name))
    (let ((buf (window-buffer (selected-window))))
      (message "refreshing imenu")
      (lsp-ui-imenu--refresh))))

(use-package lsp-mode
  :config
  (setq lsp-prefer-flymake nil)
  (setq lsp-eldoc-enable-hover nil)
  (setq lsp-enable-xref nil)
  (setq lsp-ui-sideline-enable nil)
  ;; (setq lsp-ui-doc-enable nil)
  (setq lsp-lens-enable nil)
  (setq lsp-auto-guess-root nil)
  (setq lsp-headerline-breadcrumb-enable nil)

  ;; prevent docs in minibuffer
  ;; (setq lsp-signature-auto-activate nil)
  (general-define-key
   :keymaps 'lsp-mode-map
   "C-c C-a" 'lsp-execute-code-action
   "C-c C-c C-f" 'lsp-format-buffer
   "C-c <TAB>" 'lsp-format-buffer
   "C-c l" 'c/lsp-avy-lens
   "M-." 'lsp-find-definition)
  (general-define-key
   :keymaps 'lsp-mode-map
   :states '(normal)
   "gd" 'lsp-find-definition)

  (setq lsp-ui-imenu-window-fix-width t)
  (setq lsp-ui-imenu-window-width 35)
  (add-to-list 'window-selection-change-functions #'c/lsp--refresh-imenu))

(use-package lsp-ui)

;;; eglot

(use-package eglot
  :ensure nil
  :bind (:map eglot-mode-map
         ("C-c <TAB>" . eglot-format))
  :config
  (add-hook 'eglot-managed-mode-hook
            (lambda () (eglot-inlay-hints-mode -1))))

;;; org-mode

(require 'chip-org)
(require 'chip-org-agenda)

(require 'org-protocol)

;; For plotting org-mode tables
(use-package gnuplot)
(use-package gnuplot-mode)

(use-package org-roam
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n t" . org-roam-dailies-find-today)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-dailies-capture-today))
  :init
  (setq org-roam-v2-ack t)
  :config
  ;; (setq org-roam-directory "/home/chip/tmp/roam")
  (setq org-roam-directory "~/org/personal/roam")
  (setq org-roam-dailies-directory "~/org/personal/roam/daily")
  (setq org-roam-file-extensions '("org"))

  (setq org-roam-dailies-capture-templates
        '(
          ("e" "entry" entry
           "* %?\n%T"
           :if-new (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n")
           ;; :head "#+title: %<%Y-%m-%d>\n"
           )
          ("s" "summary" entry
           "* Day summary\n%T\n%?\n\n%(org-clock-report-today)"
           :if-new (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n")
           :head "#+title: %<%Y-%m-%d>\n")
          ("w" "week summary" entry
           "* Week summary\n%T\n%?\n\n%(org-clock-report-week)"
           :if-new (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n")
           :head "#+title: %<%Y-%m-%d>\n")
          ("m" "month summary" entry
           "* Month summary\n%T\n%?\n\n%(org-clock-report-month)"
           :if-new (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n")
           :head "#+title: %<%Y-%m-%d>\n")))

  (org-roam-setup))

(use-package websocket)
(use-package simple-httpd)

(use-package org-roam-ui
  :straight
    (:host github :repo "org-roam/org-roam-ui" :branch "main" :files ("*.el" "out"))
    :after org-roam
    :config
    (setq org-roam-ui-sync-theme t
          org-roam-ui-follow t
          org-roam-ui-update-on-save t
          org-roam-ui-open-on-start t))

(use-package deft
  :after (org)
  :bind
  ("C-c n d" . deft)
  :config
  (setq deft-recursive t)
  (setq deft-use-filter-string-for-filename t)
  (setq deft-default-extension "org")
  (setq deft-directory "/home/chip/org/personal/roam")
  (setq deft-use-filename-as-title nil)
  (setq deft-extensions '("txt" "text" "md" "markdown" "org" "gpg"))
  (after-load (evil)
    (add-to-list 'evil-emacs-state-modes 'deft-mode))

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
  :defer t
  :config
  (setq shr-inhibit-images t)           ; disable image loading when viewing entries
  (setq elfeed-feeds
        `(("http://feeds.feedburner.com/blogspot/hsDu" android) ; Android Developers Blog
          ("http://oremacs.com/atom.xml" emacs)                 ; (or emacs)
          ("http://pragmaticemacs.com/feed/" emacs) ; Pragmatic Emacs
          ("https://emacsair.me/feed.xml" emacs)
          ("https://protesilaos.com/codelog.xml" emacs) ; Protesilaos Stavrou blog
          ("https://defn.io/index.xml" racket)
          "http://techsnuffle.com/feed.xml"
          "https://ultimatemachine.se/articles.xml"
          "https://hnrss.org/newest?comments=10"
          "https://lisp-journey.gitlab.io/index.xml"
          "https://lexi-lambda.github.io/feeds/all.rss.xml"
          "https://stevelosh.com/rss.xml"           ; Steve Losh
          "https://kaveh808.medium.com/feed"        ; kaveh808
          "https://nullprogram.com/feed/"           ; nullprogram
          "https://ziglang.org/news/index.xml"      ; Zig News
          "https://zig.news/feed"                   ; Zig News (unofficial)
          "https://andrewkelley.me/rss.xml"         ; Andrew Kelley
          "https://mastodon.social/@andrewrk.rss"   ; Andrew Kelley mastodon
          "https://kristoff.it/index.xml"           ; Loris Cro
          "https://blog.orhun.dev/rss.xml"          ; Orhun's blog
          "https://devlog.hexops.com/feed.xml"      ; Hexops
          "https://codewithandrea.com/rss.xml"      ; Code with Andrea
          ,(youtube-rss "UCaxar6TBM-94_ezoS00fLkA") ; Day9TV
          ;; ,(youtube-rss "UC0uTPqBCFIpZxlz_Lv1tk_g") ; Protesilaos Stavrou
          ;; ,(reddit-rss private/reddit-rss-feed "chip2n")
          "https://www.reddit.com/r/Zig/.rss" ; /r/zig
          ))

  (after-load (evil)
    (add-to-list 'evil-emacs-state-modes 'elfeed-search-mode)
    (add-to-list 'evil-emacs-state-modes 'elfeed-show-mode))

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

(setq erc-email-userid (concat private/irc-nick "/irc.libera.chat"))

(defun chip/run-erc ()
  (interactive)
  (erc-tls
   :server private/irc-server
   :port private/irc-port
   :nick private/irc-nick
   :password private/irc-password))

(use-package pdf-tools
  :config
  (pdf-loader-install)
  (add-hook 'pdf-view-mode-hook (lambda () (blink-cursor-mode -1)))
  (add-hook 'pdf-view-mode-hook 'c/pdf--setup-window))

(defun c/pdf--setup-window ()
  "Setup PDF window, removing default header and fitting height"
  ;; (set-window-fringes (selected-window) 0 0)
  ;; Seems like there's already spaced without the right fringe, so this centers the document a bit better
  (setq right-fringe-width 0)
  (setq header-line-format nil)
  (pdf-view-fit-height-to-window))

(defun c/pdf-fit-window ()
  "Resize window to document width."
  (interactive)
  (let* ((current-width (window-size (selected-window) t t nil))
         (size (car (pdf-view-image-size))))
    (window-resize (selected-window) (- size current-width) t nil t)
    (pdf-view-redisplay)))

(use-package pomidor
  :config
  (setq pomidor-sound-tick nil
        pomidor-sound-tack nil
        ;; pomidor-sound-overwork (expand-file-name (concat pomidor-dir "overwork.wav"))
	    ;; pomidor-sound-break-over (expand-file-name (concat (getenv "HOME") "/Music/overwork.wav"))
        ))

;;; Languages

;; Toggle evil emacs state when entering edebug mode
(after-load (evil-mode)
 (add-hook 'edebug-mode-hook
           (lambda ()
             (if (bound-and-true-p edebug-mode)
                 (evil-emacs-state)
               (evil-normal-state)))))

(use-package clojure-mode
  :after (lsp-mode)
  :defer t
  :hook ((clojure-mode . lsp)
         (clojurec-mode . lsp)
         (clojurescript-mode . lsp))
  :config
  (after-load (outshine-mode)
    (add-hook 'clojure-mode 'outshine-mode)
    (add-hook 'clojurescript-mode 'outshine-mode)))

(use-package cider
  :defer t
  :config
  (setq cider-test-show-report-on-success nil)
  (setq cider-auto-select-test-report-buffer nil)
  (setq cider-offer-to-open-cljs-app-in-browser nil)
  (setq cider-test-fail-fast nil)
  (eldoc-mode t)
  (after-load (evil)
    (add-to-list 'evil-motion-state-modes 'cider-test-report-mode)))

;; Always consider dir-locals setting the "dev" alias as safe
(add-to-list 'safe-local-variable-values '(cider-clojure-cli-aliases . "dev"))

(use-package inf-clojure
  :defer t)

(use-package clj-refactor
  :defer t)

(use-package anaconda-mode
  :config
  (add-hook 'python-mode-hook 'anaconda-mode)
  (c/diminish anaconda-mode))

(use-package pyvenv)

(use-package lsp-dart
  :defer t
  :config
  (setq lsp-dart-line-length 120)
  (setq lsp-dart-sdk-dir "~/flutter/bin/cache/dart-sdk")
  (setq lsp-dart-flutter-sdk-dir "~/flutter")
  (add-hook 'dart-mode-hook 'lsp)
  (setq lsp-dart-dap-flutter-hot-reload-on-save t)
  (setq lsp-dart-flutter-widget-guides nil))

(use-package dart-mode
  :after (projectile)
  :defer t
  :config
  (add-to-list 'auto-mode-alist (cons (rx ".dart" eos) 'dart-mode))
  ;; (add-hook 'dart-mode-hook 'flycheck-mode)
  (after-load (outshine-mode)
    (add-hook 'dart-mode-hook 'outshine-mode))
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
   "f" 'dart-server-format))

(defun flutter--find-project-root ()
  (locate-dominating-file (buffer-file-name) "pubspec.yaml"))

(use-package flutter
  ;; :hook (dart-mode . (lambda () (add-hook 'after-save-hook #'flutter-run-or-hot-reload nil t)))
  :bind (:map dart-mode-map
         ("C-c c" . flutter-run-or-hot-reload))
  :config
  (setq flutter-sdk-path "~/flutter"))

;; csv

(use-package csv-mode)

;; nvm

(defun chip/nvm-10.23.0 ()
  (interactive)
  (nvm-use "v10.23.0"))

(defun chip/vterm-nvm-14.18.1 ()
  (interactive)
  (nvm-use "v14.18.1" (lambda () (vterm "*vterm-nvm-14.18.1*"))))

(defun chip/vterm-nvm-10.23.0 ()
  (interactive)
  (nvm-use "v10.23.0" (lambda () (vterm "*vterm-nvm-10.23.0*"))))

(defun chip/vterm-nvm-8.17.0 ()
  (interactive)
  (nvm-use "v8.17.0" (lambda () (vterm "*vterm-nvm-8.17.0*"))))

(defvar chip/nvm-versions
  '("8.17.0" "10.23.0" "12.10.0" "12.22.1")
  "Versions available for use with NVM.")

(defun chip/vterm-nvm-pick ()
  (interactive)
  (let ((versions ))
    (let ((version (completing-read "NVM version" chip/nvm-versions)))
      (nvm-use version (lambda () (vterm (format "*vterm-nvm-%s*" version)))))))

(use-package nvm
  :disabled t
  :config
  (nvm-use "v19.2.0"))

(defun do-nvm-use (version)
  (interactive "sVersion: ")
  (nvm-use version))

(defun run-node (cwd)
  (interactive "DDirectory: ")
  (call-interactively 'do-nvm-use)
  (let ((default-directory cwd))
    (pop-to-buffer (make-comint (format "node-repl-%s" cwd) "node" nil "--interactive"))))

;; js2-mode

(use-package js2-mode
  :defer t
  :mode (("\\.js\\'" . js2-mode)
         ("\\.jsx\\'" . js2-mode))
  :config
  (setq js-indent-level 2)
  (setq js2-skip-preprocessor-directives t) ; ignore shebangs

  ;; disable semicolon warnings
  (setq js2-strict-missing-semi-warning nil)
  (setq js2-missing-semi-one-line-override t)

  ;; disable inconsistent return warnings
  (setq js2-strict-inconsistent-return-warning nil))

(use-package json-mode
  :config
  ;; add simple validation through flycheck (for missing commas etc)
  (add-hook 'json-mode-hook #'flycheck-mode))

(use-package typescript-mode
  :config
  (after-load (outshine-mode)
    (add-hook 'typescript-mode-hook 'outshine-mode)))

(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))

  ;; prevent autogenerated JS files on first save
  (setq tide-tsserver-flags '("--noEmit --resolveJsonModule"))

  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  (company-mode +1))

(use-package tide
  :after (typescript-mode)
  :config
  ;; formats the buffer before saving
  (add-hook 'before-save-hook 'tide-format-before-save)
  (add-hook 'typescript-mode-hook 'setup-tide-mode)
  (c/diminish tide-mode)
  (general-define-key
   :states 'normal
   :keymaps 'typescript-mode-map
   "gd" 'tide-jump-to-definition))

(defun c/tsc-watch (path)
  (interactive (list (read-file-name "Select file to watch with tsc: ")))
  (vterm "*tsc*")
  (c/vterm-cd (file-name-directory path))
  (vterm-insert "npx tsc -w")
  (vterm-send-return))

(use-package kotlin-mode
  :config
  (setq kotlin-tab-width 4)
  (after-load (outshine-mode)
    (add-hook 'kotlin-mode-hook 'outshine-mode)))

(use-package groovy-mode)

(use-package rustic
  :config
  (setq rustic-lsp-server 'rust-analyzer)
  (setq rustic-compile-backtrace 1)
  (add-hook 'rustic-mode-hook
            (lambda () (setq company-backends '(company-capf)))))

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

(use-package markdown-mode
  :config
  (add-hook 'markdown-mode-hook 'toggle-word-wrap))

(require 'ob-lilypond)

(use-package glsl-mode)
(use-package wgsl-mode)

(require 'bolt-mode)

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
  (c/diminish olivetti-mode)
  (setq-default olivetti-body-width 80))

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

(defun c/eat--update-hl-line ()
  (cond
   ((bound-and-true-p eat--char-mode) (hl-line-mode -1))
   ((bound-and-true-p eat--semi-char-mode) (hl-line-mode -1))
   (t (hl-line-mode 1))))

;; TODO support prefix args
(defun c/eat ()
  (interactive)
  (let ((win (get-buffer-window "*eat*")))
    (if win
        (select-window win)
      (eat))))

(defun c/eat-cd (path)
  (interactive "D")
  "Switch to eat buffer and send cd command."
  (c/eat)
  (switch-to-buffer "*eat*")
  (eat-term-send-string eat-terminal (format "cd %s" path))
  (eat-self-input 1 'return))

(use-package eat
  :straight (:host codeberg
             :repo "akib/emacs-eat"
             :files ("*.el" ("term" "term/*.el") "*.texi"
                     "*.ti" ("terminfo/e" "terminfo/e/*")
                     ("terminfo/65" "terminfo/65/*")
                     ("integration" "integration/*")
                     (:exclude ".dir-locals.el" "*-tests.el")))
  :custom
  (eat-shell-prompt-annotation-failure-margin-indicator ">")
  (eat-shell-prompt-annotation-success-margin-indicator "⚬")
  :bind (("C-c t" . c/eat)
         ("C-c T" . c/eat-cd))
  :hook ((eat--semi-char-mode . c/eat--update-hl-line)
         (eat--char-mode . c/eat--update-hl-line)))

(use-package vterm
  :defer t
  ;; :bind (("C-c t" . vterm)
  ;;        ("C-c T" . c/vterm-toggle-cd))
  :config
  (unless c/mac?
    (setq vterm-shell "fish"))
  (general-define-key
   :keymaps 'vterm-mode-map
   "<prior>" 'scroll-down-command
   "<next>" 'scroll-up-command
   "S-<prior>" 'scroll-other-window-down
   "S-<next>" 'scroll-other-window
   "M-n" 'vterm-send-down
   "M-p" 'vterm-send-up)

  ;; Unbind keys conflicting with my personal shortcuts
  (add-hook 'vterm-mode-hook
            (lambda ()
              (local-unset-key (kbd "C-b"))
              (local-unset-key (kbd "C-p"))))

  ;; line highlight flickers in vterm, so disable it
  (add-hook 'vterm-mode-hook (lambda () (setq-local global-hl-line-mode nil)))
  ;; reset window configuration when toggling back
  (setq vterm-toggle-hide-method 'reset-window-configration)
  ;; show vterm in current window
  (setq vterm-toggle-fullscreen-p nil)
  (add-to-list 'display-buffer-alist
               '((lambda (bufname _) (with-current-buffer bufname (equal major-mode 'vterm-mode)))
                 (display-buffer-reuse-window display-buffer-same-window))))

(defun c/vterm-clear-prompt ()
  (let ((beg (vterm--get-prompt-point))
        (end (vterm--get-end-of-line)))
    (vterm-delete-region beg end)))

(defun c/vterm-cd (path)
  "Switch to vterm buffer and send cd command."
  (c/vterm-clear-prompt)
  (vterm-insert (format "cd %s" path))
  (vterm-send-return))

(defun c/vterm-toggle-cd (path)
  (interactive "D")
  "Switch to vterm buffer and send cd command."
  (vterm)
  (c/vterm-cd path))

(defun c/project-vterm ()
  "Switch to vterm buffer and cd to project root."
  (interactive)
  (c/vterm-toggle-cd (project-root (project-current t))))

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

(defun increment-number-at-point (&optional arg)
  "Increment the number forward from point by 'arg'."
  (interactive "p*")
  (save-excursion
    (save-match-data
      (let (inc-by field-width answer)
        (setq inc-by (if arg arg 1))
        (skip-chars-backward "0123456789")
        (when (re-search-forward "[0-9]+" nil t)
          (setq field-width (- (match-end 0) (match-beginning 0)))
          (setq answer (+ (string-to-number (match-string 0) 10) inc-by))
          (when (< answer 0)
            (setq answer (+ (expt 10 field-width) answer)))
          (replace-match (format (concat "%0" (int-to-string field-width) "d")
                                 answer)))))))

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

(defun current-time-unix ()
  (interactive)
  (let ((time (time-convert nil 'integer)))
    (if (called-interactively-p 'interactive)
        (message (int-to-string time))
      time)))

(defun insert-current-time-unix ()
  "Inserts the number of seconds since 1970-01-01 00:00:00."
  (interactive)
  (insert (int-to-string (current-time-unix))))

(defun current-time-utc ()
  (interactive)
  (message
   (string-trim-right
    (shell-command-to-string (format "unix-to-date --utc %s" (current-time-unix))))))

(defun toggle-window-dedicated ()
  "Control whether or not Emacs is allowed to display another
buffer in current window."
  (interactive)
  (let ((window (get-buffer-window (current-buffer))))
    (set-window-dedicated-p window (not (window-dedicated-p window)))
    (if (window-dedicated-p window)
        (message "%s: Can't touch this!" (current-buffer))
      (message "%s is up for grabs." (current-buffer)))))

(defun toggle-window-size-fixed ()
  (interactive)
  (let ((window (get-buffer-window (current-buffer))))
    (setq window-size-fixed (not (window-size-fixed-p window)))
    (if (window-size-fixed-p window)
        (message "%s: Can't resize this!" (current-buffer))
      (message "%s is up for grabs." (current-buffer)))))

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

;; calfw

(use-package calfw
  :config
  (setq cfw:fchar-junction ?╋
        cfw:fchar-vertical-line ?┃
        cfw:fchar-horizontal-line ?━
        cfw:fchar-left-junction ?┣
        cfw:fchar-right-junction ?┫
        cfw:fchar-top-junction ?┯
        cfw:fchar-top-left-corner ?┏
        cfw:fchar-top-right-corner ?┓)

  ;; Iosevka has incorrect width for ellipsis character currently
  (setq truncate-string-ellipsis "..."))

(use-package calfw-org
  :config
  (setq cfw:org-face-agenda-item-foreground-color "white"))

;; Docker

(use-package dockerfile-mode)

;; web-mode

(use-package web-mode
  :config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-engines-alist '(("elixir" . "\\.ex\\'")
                                 ("elixir" . "\\.heex\\'")
                                 ("elixir" . "\\.sface\\'")))
  (add-to-list 'auto-mode-alist '("\\.sface\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.mjml\\'" . web-mode)))

;;; speed-type

(use-package speed-type
  :config
  (after-load (evil)
    (add-to-list 'evil-emacs-state-modes 'speed-type-mode)))

;;; google-this

(use-package google-this)


(provide 'chip-init-desktop)

;;; chip-init-desktop.el ends here
