(provide 'init-exwm)

(use-package exwm
  :ensure t)

(setq exwm-workspace-number 2)

(require 'exwm-randr)
(setq exwm-randr-workspace-output-plist '(0 "DVI-D-0" 1 "DVI-I-0"))
(add-hook 'exwm-randr-screen-change-hook
          (lambda ()
            (start-process-shell-command
             "xrandr" nil "xrandr --output DVI-D-0 --left-of DVI-I-0 --auto")))
(exwm-randr-enable)



(exwm-input-set-key (kbd "s-SPC") 'counsel-M-x)

(push ?\s-w exwm-input-prefix-keys)
(push ?\s-e exwm-input-prefix-keys)
(exwm-input-set-key (kbd "s-w") (lambda () (interactive) (exwm-workspace-switch 0)))
(exwm-input-set-key (kbd "s-e") (lambda () (interactive) (exwm-workspace-switch 1)))

(push ?\s-h exwm-input-prefix-keys)
(push ?\s-k exwm-input-prefix-keys)
(push ?\s-j exwm-input-prefix-keys)
(push ?\s-l exwm-input-prefix-keys)
(exwm-input-set-key (kbd "s-h") 'evil-window-left)
(exwm-input-set-key (kbd "s-k") 'evil-window-up)
(exwm-input-set-key (kbd "s-l") 'evil-window-right)
(exwm-input-set-key (kbd "s-j") 'evil-window-down)

(push ?\s-b exwm-input-prefix-keys)
(exwm-input-set-key (kbd "s-b") 'ivy-switch-buffer)

(push ?\s-\C-h exwm-input-prefix-keys)
(push ?\s-\C-j exwm-input-prefix-keys)
(push ?\s-\C-k exwm-input-prefix-keys)
(push ?\s-\C-l exwm-input-prefix-keys)
(exwm-input-set-key (kbd "s-C-h") 'split-window-right)
(exwm-input-set-key (kbd "s-C-j") (lambda () (interactive) (split-window-below) (other-window 1)))
(exwm-input-set-key (kbd "s-C-k") 'split-window-below)
(exwm-input-set-key (kbd "s-C-l") (lambda () (interactive) (split-window-right) (other-window 1)))

(push ?\s-\S-h exwm-input-prefix-keys)
(push ?\s-\S-j exwm-input-prefix-keys)
(push ?\s-\S-k exwm-input-prefix-keys)
(push ?\s-\S-l exwm-input-prefix-keys)
(exwm-input-set-key (kbd "s-S-l") 'evil-window-increase-width)
(exwm-input-set-key (kbd "s-S-h") 'evil-window-decrease-width)
(exwm-input-set-key (kbd "s-S-k") 'evil-window-increase-height)
(exwm-input-set-key (kbd "s-S-j") 'evil-window-decrease-height)

(push ?\s-1 exwm-input-prefix-keys)
(push ?\s-2 exwm-input-prefix-keys)
(push ?\s-3 exwm-input-prefix-keys)
(exwm-input-set-key (kbd "s-1") 'eyebrowse-switch-to-window-config-1)
(exwm-input-set-key (kbd "s-2") 'eyebrowse-switch-to-window-config-2)
(exwm-input-set-key (kbd "s-3") 'eyebrowse-switch-to-window-config-3)

(push ?\s-p exwm-input-prefix-keys)
(exwm-input-set-key (kbd "s-p") 'exwm-app-launcher)

(push ?\s-g exwm-input-prefix-keys)
(exwm-input-set-key (kbd "s-g") 'magit-status)

(push ?\s-\S-c exwm-input-prefix-keys)
(exwm-input-set-key (kbd "s-S-c") 'kill-buffer)



(push ?\: exwm-input-prefix-keys)




(defvar exwm-app-launcher--prompt "$ "
  "Prompt for the EXWM application launcher")
(defun exwm-app-launcher (command)
      (interactive (list (read-shell-command exwm-app-launcher--prompt)))
      (start-process-shell-command command nil command))

(defun exwm-rename-buffer ()
  (interactive)
  (exwm-workspace-rename-buffer
   (concat exwm-class-name ":"
           (if (<= (length exwm-title) 50) exwm-title
             (concat (substring exwm-title 0 49) "...")))))

;; Add these hooks in a suitable place (e.g., as done in exwm-config-default)
(add-hook 'exwm-update-class-hook 'exwm-rename-buffer)
(add-hook 'exwm-update-title-hook 'exwm-rename-buffer)


(exwm-enable)
