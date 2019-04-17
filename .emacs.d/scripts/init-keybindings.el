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
   "wf" 'delete-other-windows
   "wd" 'evil-delete-buffer))

(defun chip/setup-code-keys ()
  "Setup keybindings for code editing"
  (interactive)
  (general-define-key
   :prefix leader
   :keymaps '(normal visual emacs)
   "cl" 'comment-line
   "zl" 'hs-hide-level
   "zo" 'hs-show-block
   "zc" 'hs-hide-block
   "zz" 'hs-toggle-hiding))

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
   "P" 'projectile-switch-project
   "t" 'projectile-toggle-between-implementation-and-test))

(defun chip/setup-eyebrowse-keys ()
  "Setup keybindings for eyebrowse"
  (interactive)
  (general-define-key
   :prefix leader
   :keymaps '(normal visual emacs)
   "ww" 'eyebrowse-switch-to-window-config
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

(defun chip/setup-winner-keys ()
  "Setup keybindings for winner-mode"
  (interactive)
  (general-define-key
   :prefix leader
   :keymaps '(normal visual emacs)
   "h" 'winner-undo
   "l" 'winner-redo))

(defun chip/setup-elfeed-keys ()
  "Setup keybindings for elfeed"
  (interactive)
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
   "oa" (lambda () (interactive) (org-agenda nil "c"))
   "oA" 'org-agenda-list
   "oo" 'org-open-at-point
   "oe" 'org-capture
   "on" 'org-narrow-to-subtree
   "os" 'org-schedule
   "od" 'org-deadline
   "ot" 'org-set-tags-command
   "oci" 'org-clock-in
   "oco" 'org-clock-out
   "or" 'org-refile
   "ow" 'widen)
  (general-define-key
   :states 'normal
   "RET" 'org-open-at-point)
  (general-define-key
   :keymaps 'org-mode-map
   "M-k" 'org-move-subtree-up
   "M-j" 'org-move-subtree-down
   "M-l" 'org-demote-subtree
   "M-h" 'org-promote-subtree)
  (general-define-key
   :keymaps 'org-agenda-mode-map
   "RET" 'org-agenda-switch-to
   "j" 'next-line
   "k" 'previous-line))

(defun chip/setup-elisp-keys ()
  "Setup keybindings for emacs-lisp-mode"
  (interactive)
  (general-define-key
   :prefix leader
   :states '(normal visual)
   :keymaps 'lisp-mode-shared-map
   "er" 'eval-region
   "eb" 'eval-buffer
   "ed" 'eval-defun
   "es" 'eval-last-sexp
   "ee" 'eval-expression))

(defun chip/setup-common-lisp-keys ()
  "Setup keybindings for common-lisp-mode"
  (interactive)
  (general-define-key
   :prefix leader
   :states 'normal
   :keymaps 'slime-mode-map
   "es" 'slime-eval-last-expression
   "ed" 'slime-eval-defun))

(defun chip/setup-cider-keys ()
  "Setup keybindings for cider-mode"
  (interactive)
  (general-define-key
   :prefix leader
   :states '(normal visual)
   :keymaps 'cider-mode-map
   "er" 'cider-eval-region
   "eb" 'cider-eval-buffer
   "ed" 'cider-eval-defun-at-point
   "es" 'cider-eval-last-sexp
   "i" 'cider-format-buffer))

(defun chip/setup-inf-clojure-keys ()
  "Setup keybindings for inf-clojure-mode"
  (interactive)
  (general-define-key
   :prefix leader
   :states '(normal visual)
   :keymaps 'inf-clojure-minor-mode-map
   "er" 'inf-clojure-eval-region
   "eb" 'inf-clojure-eval-buffer
   "ed" 'inf-clojure-eval-defun
   "es" 'inf-clojure-eval-last-sexp
   "en" 'inf-clojure-set-ns))

(defun chip/setup-racket-keys ()
  "Setup keybindings for racket-mode"
  (interactive)
  (general-define-key
   :prefix leader
   :states '(normal visual)
   :keymaps 'racket-mode-map
   "er" 'geiser-eval-region
   "eb" 'geiser-eval-buffer
   "ed" 'geiser-eval-definition
   "es" 'geiser-eval-last-sexp))

(defun chip/setup-python-keys ()
  "Setup keybindings for python-mode"
  (interactive)
  (general-define-key
   :prefix leader
   :states '(normal visual)
   :keymaps 'python-mode-map
   "er" 'python-shell-send-region
   "eb" 'python-shell-send-buffer
   "ed" 'python-shell-send-defun))

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
  :after (evil ivy hydra magit company pdf-tools anaconda-mode)
  :config
  (setq leader "<SPC>")
  (chip/setup-window-keys)
  (chip/setup-code-keys)
  (chip/setup-ivy-keys)
  (chip/setup-avy-keys)
  (chip/setup-magit-keys)
  (chip/setup-projectile-keys)
  (chip/setup-eyebrowse-keys)
  (chip/setup-winner-keys)
  (chip/setup-elfeed-keys)
  (chip/setup-language-keys)
  (chip/setup-applications-keys)
  (chip/setup-org-keys)
  (chip/setup-elisp-keys)
  (chip/setup-common-lisp-keys)
  (chip/setup-cider-keys)
  (chip/setup-python-keys)
  (chip/setup-inf-clojure-keys)
  (chip/setup-racket-keys)
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
   "C" 'chip/open-config-file
   "d" 'kill-this-buffer
   "q" 'delete-window)
  (general-define-key
   :mode 'flymake-mode
   "M-n" 'flymake-goto-next-error
   "M-p" 'flymake-goto-prev-error))

(defun chip/open-config-file ()
  "Open Emacs configuration file"
  (interactive)
  (find-file "~/.emacs.d/init.el"))
