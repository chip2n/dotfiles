(provide 'init-dart)

(use-package dart-mode
  :ensure t
  :after (projectile)
  :config
  (add-to-list 'auto-mode-alist (cons (rx ".dart" eos) 'dart-mode))
  ;; (setq dart-enable-analysis-server t)
  (setq dart-sdk-path "/home/chip/flutter/bin/cache/dart-sdk/")
  (add-hook 'dart-mode-hook 'lsp)
  (add-hook 'dart-mode-hook 'flycheck-mode)
  (add-hook 'dart-mode-hook 'chip/setup-dart-keys)
  (add-hook 'dart-mode-hook (lambda ()
                              (add-hook 'after-save-hook 'flutter-hot-reload nil 'make-it-local)))
  (add-to-list 'projectile-project-root-files-bottom-up "pubspec.yaml")
  (add-to-list 'projectile-project-root-files-bottom-up "BUILD"))

(use-package dart-server
  :ensure t
  :after (dart-mode))

(defun chip/setup-dart-keys ()
  "Setup keybindings for dart mode"
  (interactive)
  (general-define-key
   :states 'normal
   :keymaps 'dart-mode-map
   "gd" 'dart-server-goto)
  (general-define-key
   :prefix leader
   :states 'normal
   :keymaps 'dart-mode-map
   "mww" 'flutter-widget-wrap-widget
   "mwc" 'flutter-widget-wrap-center
   "mwg" 'flutter-widget-wrap-group
   "mwp" 'flutter-widget-wrap-padding
   "ml" 'flutter-widget-lift
   "mf" 'dart-server-format))

(defun flutter--find-project-root ()
  (locate-dominating-file (buffer-file-name) "pubspec.yaml"))

(defun flutter-run ()
  (interactive)
  (let ((project-root (flutter--find-project-root)))
    (if (not project-root)
        (error "Not inside a flutter project (no pubspec.yaml found in any parent directory)."))
    (pop-to-buffer (get-buffer-create (generate-new-buffer-name "*flutter*")))
    (cd project-root)
    (shell (current-buffer))
    (process-send-string nil "flutter run -d all --pid-file /tmp/flutter.pid\n")
    (evil-normal-state)
    (other-window -1)))

(defun flutter-hot-reload ()
  "Triggers a hot reload of the running flutter application"
  (interactive)
  (shell-command "kill -SIGUSR1 $(cat /tmp/flutter.pid)"))

(defun flutter-hot-restart ()
  "Triggers a hot restart of the running flutter application"
  (interactive)
  (shell-command "kill -SIGUSR2 $(cat /tmp/flutter.pid)"))

(defun flutter--move-beginning-of-widget ()
  (re-search-backward (rx space))
  (forward-char 1))

(defun flutter--move-end-of-widget ()
  (forward-list 1))

(defun flutter-select-widget-at-point ()
  (interactive)
  (flutter--move-beginning-of-widget)
  (set-mark-command nil)
  (flutter--move-end-of-widget)
  (setq deactivate-mark nil))

(defun flutter-widget-wrap-padding ()
  (interactive)
  (flutter-select-widget-at-point)
  (yas-expand-snippet (yas-lookup-snippet "padding"))
  (evil-insert 1))

(defun flutter-widget-wrap-center ()
  (interactive)
  (flutter-select-widget-at-point)
  (yas-expand-snippet (yas-lookup-snippet "center"))
  (evil-insert 1))

(defun flutter-widget-wrap-widget ()
  (interactive)
  (flutter-select-widget-at-point)
  (yas-expand-snippet (yas-lookup-snippet "widget"))
  (evil-insert 1))

(defun flutter-widget-wrap-group ()
  (interactive)
  (flutter-select-widget-at-point)
  (yas-expand-snippet (yas-lookup-snippet "group"))
  (evil-insert 1))

(defmacro flutter--widget-execute-region (fun)
  "Run function on region (must take beginning and end as last two arguments"
  `(save-excursion
     (flutter--move-beginning-of-widget)
     (let ((beg (point)))
       (flutter--move-end-of-widget)
       (,fun beg (point)))))

(defun flutter-widget-kill ()
  (interactive)
  (flutter--widget-execute-region kill-region))

(defun flutter-widget-delete ()
  (interactive)
  (flutter--widget-execute-region delete-region))

(defun flutter-widget-copy ()
  (interactive)
  (flutter--widget-execute-region copy-region-as-kill))

(defun flutter--move-beginning-of-parent-widget ()
  (interactive)
  (backward-char 1)
  (re-search-backward (rx (syntax open-parenthesis)))
  (flutter--move-beginning-of-widget))

(defun flutter-widget-lift ()
  (interactive)
  (flutter-widget-copy)
  (flutter--move-beginning-of-parent-widget)
  (flutter-widget-delete)
  (yank))
