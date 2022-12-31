;;; chip-vc.el -*- lexical-binding: t -*-

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

;;; Commentary:

;; This file contains code for working with version control systems.

;;; Code:

;;; Package: magit

(defun chip/magit-status-root-dir (dir)
  (magit-status (vc-find-root dir ".git")))

(defun c/magit-status-here ()
  (interactive)
  (magit-status-here)
  (recenter))

(use-package magit
  :bind (("C-x g" . magit-status)
         ("C-x G" . c/magit-status-here))

  :general (:keymaps 'magit-blame-mode-map
            :states 'normal
            "RET" 'magit-show-commit
            "q" 'magit-blame-quit)

  :config
  (setq magit-clone-default-directory chip-dev-dir)

  ;; use C-<tab> for switching windows
  (define-key magit-status-mode-map (kbd "C-<tab>") nil)

  ;; don't show line numbers in magit buffers
  (add-hook 'magit-mode-hook (lambda () (display-line-numbers-mode -1)))

  ;; show magit in current window
  (setq magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)

  ;; start magit commit buffers in evil insert mode
  (after-load (evil)
    (add-hook 'git-commit-mode-hook 'evil-insert-state))

  ;; add magit as ivy actions
  (after-load (ivy counsel projectile)
    (ivy-add-actions
     'counsel-projectile
     '(("v" chip/magit-status-root-dir "magit")))
    (ivy-add-actions
     'counsel-projectile-find-file
     '(("v" chip/magit-status-root-dir "magit")))))

(use-package forge
  :after magit
  :config
  (after-load (evil)
    (add-to-list 'evil-emacs-state-modes 'forge-topic-mode)))

(use-package ssh-agency
  :config
  (setq ssh-agency-keys '("~/.ssh/github")))

;;; Package: git-gutter

;; see added/deleted/modified lines directly in the buffer, as well as staging/unstaging hunks quickly
(use-package git-gutter
  :config
  (setq git-gutter:added-sign " +")
  (setq git-gutter:deleted-sign " -")
  (setq git-gutter:modified-sign " ~")

  (after-load (diminish)
    (diminish 'git-gutter-mode)))

(provide 'chip-vc)

;;; chip-vc.el ends here
