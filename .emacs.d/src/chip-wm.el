;;; chip-wm.el -*- lexical-binding: t -*-

;; Copyright (C) 2022  Andreas Arvidsson
;;
;; Author: Andreas Arvidsson <andreas@arvidsson.io>
;; Keywords: config
;;
;; This file is not part of GNU Emacs
;;
;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Code:

(use-package exwm
  :ensure t)

(setq exwm-workspace-number 2)

(require 'exwm-randr)
(setq exwm-randr-workspace-output-plist '(1 "DVI-D-0" 0 "DVI-I-0"))
(add-hook 'exwm-randr-screen-change-hook
          (lambda ()
            (start-process-shell-command
             "xrandr" nil "xrandr --output DVI-D-0 --left-of DVI-I-0 --auto")))
(exwm-randr-enable)

;; (exwm-input-set-key (kbd "s-SPC") 'counsel-M-x)

(setq exwm-input-global-keys
      '((,(kbd "C-b") . chip/switch-buffer)))

(push ?\s-w exwm-input-prefix-keys)
(push ?\s-e exwm-input-prefix-keys)
(exwm-input-set-key (kbd "s-q") (lambda () (interactive) (exwm-workspace-switch 0)))
(exwm-input-set-key (kbd "s-w") (lambda () (interactive) (exwm-workspace-switch 1)))

(push ?\s-h exwm-input-prefix-keys)
(push ?\s-k exwm-input-prefix-keys)
(push ?\s-j exwm-input-prefix-keys)
(push ?\s-l exwm-input-prefix-keys)
(exwm-input-set-key (kbd "s-h") 'evil-window-left)
(exwm-input-set-key (kbd "s-n") 'evil-window-down)
(exwm-input-set-key (kbd "s-e") 'evil-window-up)
(exwm-input-set-key (kbd "s-i") 'evil-window-right)

(push ?\s-C exwm-input-prefix-keys)
(exwm-input-set-key (kbd "s-C") 'kill-buffer-and-window)

(push ?\s-b exwm-input-prefix-keys)
(exwm-input-set-key (kbd "s-b") 'chip/switch-buffer)

(push ?\s-\C-h exwm-input-prefix-keys)
(push ?\s-\C-j exwm-input-prefix-keys)
(push ?\s-\C-k exwm-input-prefix-keys)
(push ?\s-\C-l exwm-input-prefix-keys)
(exwm-input-set-key (kbd "s-C-h") 'split-window-right)
(exwm-input-set-key (kbd "s-C-n") (lambda () (interactive) (split-window-below) (other-window 1)))
(exwm-input-set-key (kbd "s-C-e") 'split-window-below)
(exwm-input-set-key (kbd "s-C-i") (lambda () (interactive) (split-window-right) (other-window 1)))

(push ?\s-H exwm-input-prefix-keys)
(push ?\s-J exwm-input-prefix-keys)
(push ?\s-K exwm-input-prefix-keys)
(push ?\s-L exwm-input-prefix-keys)
(exwm-input-set-key (kbd "s-H") 'evil-window-decrease-width)
(exwm-input-set-key (kbd "s-N") 'evil-window-decrease-height)
(exwm-input-set-key (kbd "s-E") 'evil-window-increase-width)
(exwm-input-set-key (kbd "s-I") 'evil-window-increase-height)

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

(provide 'chip-wm)

;;; chip-wm.el ends here
