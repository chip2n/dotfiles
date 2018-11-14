(provide 'init-dart)

(use-package dart-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist (cons (rx ".dart" eos) 'dart-mode))
  (setq dart-enable-analysis-server t)
  (setq dart-sdk-path "/home/chip/flutter/bin/cache/dart-sdk/")
  (add-hook 'dart-mode-hook 'flycheck-mode)
  (add-hook 'dart-mode-hook 'chip/setup-dart-keys)
  (add-hook 'dart-mode-hook (lambda ()
                              (add-hook 'after-save-hook 'chip/flutter-hot-reload nil 'make-it-local)))
  ;; Hack to enable syntax highlighting of single quoted strings.
  ;; https://github.com/bradyt/dart-mode/issues/47
  ;; https://github.com/bradyt/dart-mode/pull/55#issuecomment-397411088
  ;;
  ;; Doing this may screw up other buffers using cc-mode I guess, but fuck it.
  (defun c-parse-quotes-after-change (beg end &rest rest) nil)

  ;; Use dart-sdk-path to find dartfmt. Pull request not merged yet.
  ;; https://github.com/bradyt/dart-mode/pull/66
  (defun dart-formatter-command ()
    (or dart-formatter-command-override
        (when dart-sdk-path
          (concat dart-sdk-path
                  (file-name-as-directory "bin")
                  (if (memq system-type '(ms-dos windows-nt))
                      "dartfmt.exe"
                    "dartfmt"))))))

(defun chip/setup-dart-keys ()
  "Setup keybindings for dart mode"
  (interactive)
  (general-define-key
   :states 'normal
   :keymaps 'dart-mode-map
   "gd" 'dart-goto))

(defun chip/flutter-hot-reload ()
  "Triggers a hot reload of the running flutter application"
  (interactive)
  (shell-command "kill -SIGUSR1 $(cat /tmp/flutter.pid)"))

(defun chip/flutter-hot-restart ()
  "Triggers a hot restart of the running flutter application"
  (interactive)
  (shell-command "kill -SIGUSR2 $(cat /tmp/flutter.pid)"))
