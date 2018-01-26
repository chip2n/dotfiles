(provide 'init-python)

;; anaconda-mode: https://github.com/proofit404/anaconda-mode
(use-package anaconda-mode
  :ensure t
  :config
  (add-hook 'python-mode-hook 'anaconda-mode))

;; pyvenv: https://github.com/jorgenschaefer/pyvenv
(use-package pyvenv
  :ensure t)

;; Bug fix: http://emacs.stackexchange.com/questions/30082/your-python-shell-interpreter-doesn-t-seem-to-support-readline
;; Should be fixed in emacs version 25.2
(with-eval-after-load 'python
  (defun python-shell-completion-native-try ()
    "Return non-nil if can trigger native completion."
    (let ((python-shell-completion-native-enable t)
          (python-shell-completion-native-output-timeout
           python-shell-completion-native-try-output-timeout))
      (python-shell-completion-native-get-completions
       (get-buffer-process (current-buffer))
       nil "_"))))
