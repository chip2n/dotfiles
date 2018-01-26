(provide 'init-keybindings)

(defun chip/setup-window-keys ()
  "Setup keybindings for window management"
  (interactive)
  (general-define-key
   :keymaps '(normal visual)
   "C-h" 'windmove-left
   "C-k" 'windmove-up
   "C-l" 'windmove-right
   "C-j" 'windmove-down
   "C-S-l" 'evil-window-increase-width
   "C-S-h" 'evil-window-decrease-width
   "C-S-k" 'evil-window-increase-height
   "C-S-j" 'evil-window-decrease-height)
  (general-define-key
   :prefix leader
   :keymaps '(normal)
   "wh" 'split-window-right
   "wl" (lambda () (interactive) (split-window-right) (other-window 1))
   "wk" 'split-window-below
   "wj" (lambda () (interactive) (split-window-below) (other-window 1))
   "wf" 'delete-other-windows))

(defun chip/setup-code-keys ()
  "Setup keybindings for code editing"
  (interactive)
  (general-define-key
   :prefix leader
   :keymaps '(normal visual emacs)
   "zl" 'hs-hide-level
   "zo" 'hs-show-block
   "zc" 'hs-hide-block
   "zz" 'hs-toggle-hiding)
  )

(defun chip/setup-ivy-keys ()
  "Setup keybindings for ivy"
  (interactive)
  (general-define-key
   :prefix leader
   :keymaps '(normal visual emacs)
   "b" 'ivy-switch-buffer
   "B" 'ivy-switch-buffer-other-window
   "f" 'counsel-find-file)
  (general-define-key
   :keymaps '(ivy-minibuffer-map)
   "C-o" 'chip/hydra-ivy/body)
  (define-key ivy-minibuffer-map (kbd "<escape>") 'minibuffer-keyboard-quit))

(defun chip/setup-avy-keys ()
  "Setup keybindings for avy"
  (interactive)
  (general-define-key
   :prefix leader
   :keymaps '(normal visual emacs)
   "s" 'avy-goto-char-timer))

(defun chip/setup-magit-keys ()
  "Setup keybindings for magit"
  (interactive)
  (general-define-key
   :prefix leader
   :keymaps '(normal visual emacs)
   "g" 'magit-status))

(defun chip/setup-projectile-keys ()
  "Setup keybindings for projectile"
  (interactive)
  (general-define-key
   :prefix leader
   :keymaps '(normal visual emacs)
   "p" 'projectile-find-file
   "P" 'projectile-switch-project))

(defun chip/setup-eyebrowse-keys ()
  "Setup keybindings for eyebrowse"
  (interactive)
  (general-define-key
   :prefix leader
   :keymaps '(normal visual emacs)
   "w0" 'eyebrowse-switch-to-window-config-0
   "w1" 'eyebrowse-switch-to-window-config-1
   "w2" 'eyebrowse-switch-to-window-config-2
   "w3" 'eyebrowse-switch-to-window-config-3
   "w4" 'eyebrowse-switch-to-window-config-4
   "w5" 'eyebrowse-switch-to-window-config-5
   "w6" 'eyebrowse-switch-to-window-config-6
   "w7" 'eyebrowse-switch-to-window-config-7
   "w8" 'eyebrowse-switch-to-window-config-8
   "w9" 'eyebrowse-switch-to-window-config-9
   "wc" 'eyebrowse-close-window-config
   "wn" 'eyebrowse-rename-window-config))

(defun chip/setup-elfeed-keys ()
  "Setup keybindings for elfeed"
  (interactive)
  (general-define-key
   :states 'normal
   :keymaps '(elfeed-search-mode-map)
   "o" 'elfeed-search-show-entry))

(defun chip/setup-company-keys ()
  "Setup keybindings for company mode"
  (interactive)
  (general-define-key
   :keymaps '(insert)
   "<tab>" 'company-complete))

(defun chip/setup-applications-keys ()
  "Setup keybindings for miscellaneous applications under common prefix"
  (interactive)
  (general-define-key
   :prefix leader
   :states 'normal
   "ae" 'elfeed))

(use-package general
  :ensure t
  :after (evil ivy hydra magit company)
  :config
  (setq leader "<SPC>")
  (chip/setup-window-keys)
  (chip/setup-code-keys)
  (chip/setup-ivy-keys)
  (chip/setup-avy-keys)
  (chip/setup-magit-keys)
  (chip/setup-projectile-keys)
  (chip/setup-eyebrowse-keys)
  (chip/setup-elfeed-keys)
  (chip/setup-company-keys)
  (chip/setup-applications-keys)
  ;; misc
  (general-define-key
   :prefix leader
   :keymaps '(normal visual emacs)
   "<SPC>" 'execute-extended-command
   "c" 'chip/open-config-file
   "er" 'eval-region
   "ed" 'eval-defun
   "es" 'eval-last-sexp
   "ee" 'eval-expression))

(defun chip/open-config-file ()
  "Open Emacs configuration file"
  (interactive)
  (find-file "~/.emacs.d/init.el"))
