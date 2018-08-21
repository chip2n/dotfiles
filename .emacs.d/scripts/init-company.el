(provide 'init-company)

(defun chip/setup-company-keys ()
  "Setup keybindings for company mode"
  (interactive)
  (general-define-key
   :states 'insert
   :keymaps 'company-mode-map
   "C-j" 'company-select-next
   "C-k" 'company-select-previous))

(use-package company
  :ensure t
  :config
  (global-company-mode)
  (add-hook 'company-mode-hook 'chip/setup-company-keys)
  ;; prevent downcasing when autocompleting
  (setq company-dabbrev-downcase nil))

(use-package company-anaconda
  :ensure t
  :after (company anaconda-mode)
  :config
  (add-to-list 'company-backends 'company-anaconda))
