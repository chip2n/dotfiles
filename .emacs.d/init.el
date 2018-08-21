(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(package-initialize)

(add-to-list 'load-path "~/.emacs.d/scripts")
;; (require 'init-exwm)
(require 'init-use-package)
(require 'init-keybindings)
(require 'init-theme)
(require 'init-evil)
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
(require 'init-javascript)
(require 'init-dart)
(require 'init-python)
(require 'init-csharp)
(require 'init-kotlin)
(require 'init-groovy)
(require 'init-jade)
(require 'init-clojure)
(require 'init-lispy)
(require 'init-rust)
;; (require 'init-graphviz)
(require 'init-pdf)
(require 'init-pamparam)
;; (require 'init-japanese)
(require 'init-neotree)
(require 'bolt-mode)

(use-package multi-term
  :ensure t
  :config
  (setq multi-term-program "/bin/bash"))

(use-package ein
  :ensure t)

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
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#2e3436" "#a40000" "#4e9a06" "#c4a000" "#204a87" "#5c3566" "#67acd5" "#eeeeec"])
 '(custom-safe-themes
   (quote
    ("c19704688cdd4e8163e8c511aa2afdeca54634f92bd02f6b98146568a6db61e1" "21caa19a2fd655d15feca7262933665f7032f340e40eb7b99356fbd6257e6255" "d883647237bb3131eb73b526c33c267a7843d0bf2b31c2f6636bc73fa6caf298" "78dd58f09c566c10a2a9302e6bda28fbaa639cc8126e666dd1bc197892a1d572" "72e5ca520d349a5f6d8894e781f7bda3772bc5f6c0b8572dbecffb8a379830a2" "578d25e468800316d9f0b592da142f02b5f6bb21033455e0aed1ed1588cb4aba" default)))
 '(package-selected-packages
   (quote
    (ob-http yaml-mode xref-js2 writeroom-mode worf use-package toml-mode telephone-line stylus-mode smex slack ranger pyvenv pug-mode projectile pdf-tools ox-reveal org-bullets omnisharp neotree multi-term markdown-mode kotlin-mode json-mode js2-refactor ivy-hydra gruvbox-theme groovy-mode general find-file-in-project eyebrowse evil-surround evil-snipe evil-org evil-magit evil-lispy elogcat elfeed ein diminish counsel company-anaconda clojure-mode base16-theme all-the-icons ag)))
 '(safe-local-variable-values (quote ((org-use-tag-inheritance)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
