(provide 'init-evil)

(setq evil-want-keybinding nil)

(use-package evil
  :ensure t
  :init
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-d-scroll t)
  (setq evil-disable-insert-state-bindings t)
  :config
  (evil-mode 1)
  ;; allow cursor to move past last character - useful in lisp for
  ;; evaluating last sexp
  ;; (setq evil-move-cursor-back t)
  (setq evil-move-beyond-eol t)
  ;; (add-to-list 'evil-motion-state-modes 'org-agenda-mode)
  )

(use-package evil-surround
  :ensure t
  :after (evil)
  :config
  (global-evil-surround-mode 1))

(use-package evil-visualstar
  :ensure t
  :after (evil)
  :config
  (global-evil-visualstar-mode))

(use-package evil-collection
  :ensure t
  :config
  (evil-collection-init 'dired)
  (evil-collection-init 'cider))

;; Make sure shell starts in normal mode
;; TODO is shell-mode
(add-to-list 'evil-normal-state-modes 'shell-mode)
(delete 'shell-mode evil-insert-state-modes)
(add-to-list 'evil-emacs-state-modes 'image-mode)

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
         :open-rec show-subtree :close hide-subtree :close-level hide-leaves)
        )
      )
