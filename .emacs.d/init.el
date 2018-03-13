(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(package-initialize)

(add-to-list 'load-path "~/.emacs.d/scripts")
(require 'init-use-package)
(require 'init-theme)
(require 'init-evil)
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
(require 'init-eyebrowse)
(require 'init-yasnippet)
(require 'init-diminish)
(require 'init-snipe)
(require 'init-markdown)
(require 'init-yaml)
(require 'init-toml)
(require 'init-javascript)
(require 'init-python)
(require 'init-csharp)
(require 'init-kotlin)
(require 'init-groovy)
(require 'init-jade)
(require 'init-pdf)
(require 'init-keybindings)
(require 'init-pamparam)
(require 'init-japanese)
(require 'bolt-mode)

(use-package ein
  :ensure t)


(use-package ranger
  :ensure t)
(evil-leader/set-key
  "r" 'ranger)
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
