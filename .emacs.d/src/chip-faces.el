;;; chip-faces.el -*- lexical-binding: t -*-

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

(require 'chip-colors-base)

(defface chip-face-default nil
  ""
  :group 'chip)

(defface chip-face-discrete nil
  ""
  :group 'chip)

;; (defface chip-face-header-line nil
;;   ""
;;   :group 'chip)

;; (defface chip-face-header-line-inactive nil
;;   ""
;;   :group 'chip)

(defface chip-face-evil-state-normal nil
  ""
  :group 'chip)

(defface chip-face-evil-state-insert nil
  ""
  :group 'chip)

(defface chip-face-evil-state-visual nil
  ""
  :group 'chip)

(defface chip-face-evil-state-emacs nil
  ""
  :group 'chip)

(defun chip-faces ()
  ""
  (set-face-attribute 'chip-face-default nil
                      :foreground chip-color-foreground
                      :background nil)

  (set-face-attribute 'chip-face-discrete nil
                      :foreground chip-color-discrete
                      :background nil)

  ;; (set-face-attribute 'chip-face-header-line nil
  ;;                     :foreground nil
  ;;                     :background nil
  ;;                     :box `(:line-width 4 :color ,chip-color-background :style nil)
  ;;                              :background chip-color-background :foreground nil
  ;;                              )

  ;; (set-face-attribute 'chip-face-header-line-inactive nil
  ;;                     :foreground chip-color-discrete
  ;;                     :background nil
  ;;                     :box `(:line-width 4 :color ,chip-color-background :style nil)
  ;;                              :background chip-color-background :foreground chip-color-discrete
  ;;                              )

  (set-face-attribute 'chip-face-evil-state-normal nil
                      :foreground nil
                      :background nil
                      )

  (set-face-attribute 'chip-face-evil-state-insert nil
                      :foreground chip-color-blue
                      :background nil
                      )

  (set-face-attribute 'chip-face-evil-state-visual nil
                      :foreground chip-color-blue
                      :background nil
                      )

  (set-face-attribute 'chip-face-evil-state-emacs nil
                      :foreground chip-color-pink
                      :background nil
                      ))

(provide 'chip-faces)

;;; chip-faces.el ends here
