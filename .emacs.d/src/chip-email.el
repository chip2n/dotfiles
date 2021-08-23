;;; chip-email.el -*- lexical-binding: t -*-

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

;;; Commentary:

;; This file contains code for reading and writing emails.

;;; Code:

(use-package notmuch
  :config
  (setq notmuch-show-logo nil)
  ;; Remove html colors from UI
  (setq shr-use-colors nil
        shr-use-fonts nil))

;; (use-package mu4e
;;   :straight (:host github
;;              :repo "djcb/mu"
;;              :branch "master"
;;              :files ("mu4e/*")
;;              :pre-build (("./autogen.sh") ("make")))
;;   :custom (mu4e-mu-binary (expand-file-name "mu/mu" (straight--repos-dir "mu")))
;;   :config
;;   (setq mu4e-maildir "~/.mail")
;;   (setq mu4e-attachment-dir "~/Downloads")
;;   (setq mu4e-get-mail-command "mbsync protonmail")
;;   (setq mu4e-change-filenames-when-moving t) ; needed for mbsync
;;   (setq mu4e-update-interval 120)            ; update every 2 minutes
;;   )

;; Sending email

(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-auth-credentials "~/.authinfo.gpg"
      smtpmail-smtp-server "127.0.0.1"
      smtpmail-stream-type 'starttls
      smtpmail-smtp-service 1025)

(require 'gnutls)
(add-to-list 'gnutls-trustfiles (expand-file-name "~/.config/protonmail/bridge/cert.pem"))

(provide 'chip-email)

;;; chip-email.el ends here
