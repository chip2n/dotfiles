(provide 'init-dart)

(use-package dart-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist (cons (rx ".dart" eos) 'dart-mode))
  (setq dart-enable-analysis-server t)
  (setq dart-sdk-path "/home/chip/flutter/bin/cache/dart-sdk/")
  (add-hook 'dart-mode-hook 'flycheck-mode)
  (add-hook 'dart-mode-hook 'chip/setup-dart-keys))

(defun chip/setup-dart-keys ()
  "Setup keybindings for dart mode"
  (interactive)
  (general-define-key
   :states 'normal
   :keymaps 'dart-mode-map
   "gd" 'dart-goto))

;; (require 'dart-mode)
;; (add-to-list 'auto-mode-alist (cons (rx ".dart" eos) 'dart-mode))
;; (setq dart-enable-analysis-server t)
;; (setq dart-sdk-path "/home/chip/flutter/bin/cache/dart-sdk/")
