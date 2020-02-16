(general-define-key
 :map 'emacs-lisp-mode-map
 "C-c C-c" 'eval-defun)

(add-hook 'emacs-lisp-mode-hook 'company-mode)

(provide 'init-elisp)
