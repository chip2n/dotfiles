(provide 'init-company)

(defun chip/setup-company-keys ()
  "Setup keybindings for company mode"
  (interactive)
  ;; (general-define-key
  ;;  :states '(insert lispy)
  ;;  :keymaps 'company-mode-map
  ;;  "C-j" 'company-complete-common-or-cycle
  ;;  "C-k" 'company-select-previous)
  )

(use-package company
  :ensure t
  :config
  (global-company-mode)
  (add-hook 'company-mode-hook 'chip/setup-company-keys)
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

(use-package company-anaconda
  :ensure t
  :after (company anaconda-mode)
  :config
  (add-to-list 'company-backends 'company-anaconda))

(defun complete-complete-cycle-next (arg)
  (company-complete-common-or-cycle))

(defun complete-complete-cycle-previous (arg)
  (company-complete-common-or-cycle -1))
