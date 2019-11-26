(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(package-initialize)
(add-to-list 'load-path "~/.emacs.d/scripts")
(require 'init-use-package)

(setenv "NODE_PATH" "/usr/lib/node_modules")

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
(require 'init-avy)
(require 'init-hydra)
(require 'init-magit)
(require 'init-dired)
(require 'init-erc)
(require 'init-elfeed)
(require 'init-slack)
(require 'init-projectile)
(require 'init-org)
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
(require 'init-lisp)
(require 'init-lispy)
(require 'init-clojure)
(require 'init-scheme)
(require 'init-rust)
(require 'init-elixir)
(require 'init-android)
;; (require 'init-graphviz)
(require 'init-pdf)
(require 'init-pamparam)
(require 'init-pomidor)
(require 'init-lilypond)
;; (require 'init-japanese)
(require 'init-neotree)
(require 'init-google-translate)
(require 'init-quelpa)
(require 'init-remente)
(require 'bolt-mode)
(require 'vasttrafik)

;; disable C-d keybinding in comint-mode
;; (enables scrolling in shell, repls etc)
(with-eval-after-load 'comint
  (define-key comint-mode-map "\C-d" nil))

(use-package multi-term
  :ensure t
  :config
  (setq multi-term-program "/bin/bash"))

(use-package ranger
  :ensure t
  :config
  ;; make ranger the default file browser
  ;; (ranger-override-dired-mode t)

  ;; disable file preview by default
  (setq ranger-preview-file nil)

  ;; hide hidden files by default
  (setq ranger-show-hidden nil))

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

;; TODO input via org calendar
(defun insert-unix-timestamp (d)
  (interactive "nDate: ")
  (if (region-active-p) (delete-region (region-beginning) (region-end)))
  (insert
   (car (split-string (shell-command-to-string (format "timestamp %s" d)) "\n"))))

(use-package zoom
  :ensure t
  :config
  (setq zoom-size '(0.618 . 0.618)))

(use-package hyperbole
  :ensure t
  :config
  (setq hycontrol-help-flag nil)
  (setq hycontrol-invert-mode-line-flag nil))

(use-package golden-ratio-scroll-screen
  :ensure t
  :config
  (setq golden-ratio-scroll-highlight-flag nil))

(setq select-enable-clipboard t)

(add-hook 'image-mode-hook 'auto-revert-mode)

(defhydra hydra-window ()
  ("h" windmove-left)
  ("j" windmove-down)
  ("k" windmove-up)
  ("l" windmove-right)
  ("s" shrink-window)
  ("g" enlarge-window)
  ("n" shrink-window-horizontally)
  ("w" enlarge-window-horizontally)
  ("d" delete-window)
  ("u" winner-undo)
  ("r" winner-redo)
  ;; ("s" split-window-below)
  ;; ("S" split-window-right)
  ("o" other-window))

(setq inhibit-startup-echo-area-message "chip")
