(provide 'init-keybindings)

(defun chip/setup-window-keys ()
  "Setup keybindings for window management"
  (interactive)
  (general-define-key
   :keymaps '(normal visual motion)
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
   "C-o" 'chip/hydra-ivy/body
   "C-j" 'ivy-next-line
   "C-k" 'ivy-previous-line)
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

(defun chip/setup-language-keys ()
  "Setup keybindings for languages"
  (interactive)
  (general-define-key
   :prefix leader
   :states 'normal
   "j" 'uim-mode))

(defun chip/setup-applications-keys ()
  "Setup keybindings for miscellaneous applications under common prefix"
  (interactive)
  (general-define-key
   :prefix leader
   :states 'normal
   "ae" 'elfeed))

(defun chip/setup-org-keys ()
  "Setup keybindings for org mode"
  (interactive)
  (general-define-key
   :prefix leader
   :states 'normal
   "oo" 'org-open-at-point
   "oe" 'org-latex-export-to-pdf
   "on" 'org-narrow-to-subtree
   "ow" 'widen)
  (general-define-key
   :states 'normal
   "RET" 'org-open-at-point))

(defun chip/setup-multi-term-keys ()
  "Setup keybindings for multi-term"
  (interactive)
  (general-define-key
   :prefix leader
   :states 'normal
   "tl" 'multi-term-next
   "th" 'multi-term-prev))

(defun chip/setup-elisp-keys ()
  "Setup keybindings for emacs-lisp-mode"
  (interactive)
  (general-define-key
   :prefix leader
   :states 'normal
   :keymaps 'emacs-lisp-mode-map
   "er" 'eval-region
   "eb" 'eval-buffer
   "ed" 'eval-defun
   "es" 'eval-last-sexp
   "ee" 'eval-expression))

(defun chip/setup-clojure-keys ()
  "Setup keybindings for clojure-mode"
  (interactive)
  (general-define-key
   :prefix leader
   :states 'normal
   :keymaps 'clojure-mode-map
   "er" 'cider-eval-region
   "eb" 'cider-eval-buffer
   "ed" 'cider-eval-defun-at-point
   "es" 'cider-eval-last-sexp))

(defun chip/setup-pdf-tools-keys ()
  "Setup keybindings for pdf-view-mode"
  (interactive)
  (general-define-key
   :states 'normal
   :keymaps 'pdf-view-mode-map
   "j" 'pdf-view-next-line-or-next-page
   "k" 'pdf-view-previous-line-or-previous-page
   "gg" 'pdf-view-first-page
   "G" 'pdf-view-last-page
   "C-u" 'pdf-view-scroll-down-or-previous-page
   "C-d" 'pdf-view-scroll-up-or-next-page))

(use-package general
  :ensure t
  :after (evil ivy hydra magit company pdf-tools)
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
  (chip/setup-language-keys)
  (chip/setup-applications-keys)
  (chip/setup-org-keys)
  (chip/setup-multi-term-keys)
  (chip/setup-elisp-keys)
  (chip/setup-clojure-keys)
  (chip/setup-pdf-tools-keys)
  ;; misc
  (general-define-key
   :keymaps '(Info-mode-map)
   "<SPC>" nil)
  (define-key Info-mode-map (kbd "<SPC>") nil)
  (general-define-key
   :prefix leader
   :keymaps '(normal visual emacs)
   "<SPC>" 'execute-extended-command
   "c" 'chip/open-config-file))

(defun chip/open-config-file ()
  "Open Emacs configuration file"
  (interactive)
  (find-file "~/.emacs.d/init.el"))
