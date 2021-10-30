;;; chip-evil -*- lexical-binding: t -*-

;; Copyright (C) 2021  Andreas Arvidsson
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

(use-package evil-nerd-commenter
  :bind (:map prog-mode-map
              (("C-;" . evilnc-comment-or-uncomment-lines))))

(use-package evil
  :init
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll nil)
  (setq evil-want-C-d-scroll t)
  (setq evil-disable-insert-state-bindings t)
  :config
  (evil-mode 1)
  ;; allow cursor to move past last character - useful in lisp for
  ;; evaluating last sexp
  ;; (setq evil-move-cursor-back t)
  (setq evil-move-beyond-eol t)
  (evil-set-initial-state 'help-mode 'emacs)
  (evil-set-initial-state 'Info-mode 'emacs)
  (evil-set-initial-state 'image-mode 'emacs)
  (evil-set-initial-state 'special-mode 'emacs)

  )

(use-package evil-visualstar
  :after (evil)
  :config
  (global-evil-visualstar-mode))

(use-package evil-collection
  :after (evil)
  :config
  (evil-collection-init 'dired)
  (evil-collection-init 'cider)
  (evil-collection-init 'vterm)
  (c/diminish evil-collection-unimpaired-mode))

(setq evil-fold-list
      '(((hs-minor-mode)
         :open-all hs-show-all :close-all hs-hide-all :toggle hs-toggle-hiding :open hs-show-block :open-rec nil :close hs-hide-block :close-level my-hs-hide-level)
        ((hide-ifdef-mode)
         :open-all show-ifdefs :close-all hide-ifdefs :toggle nil :open show-ifdef-block :open-rec nil :close hide-ifdef-block)
        ((outline-mode outline-minor-mode org-mode markdown-mode)
         :open-all show-all :close-all
         #[nil "\300\301!\207"
               [hide-sublevels 1]
               2]
         :toggle outline-toggle-children :open
         #[nil "\300 \210\301 \207"
               [show-entry show-children]
               1]
         :open-rec show-subtree :close hide-subtree :close-level hide-leaves)))

(provide 'chip-evil)

;;; chip-evil.el ends here
