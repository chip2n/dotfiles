;;; chip-email.el -*- lexical-binding: t -*-

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

;; This file contains code for reading and writing emails.

;;; Code:

(use-package notmuch
  :defer t
  :config
  (setq notmuch-show-logo nil)
  (setq notmuch-show-header-line t)
  (setq-default notmuch-search-oldest-first nil)

  (setq notmuch-hello-sections
        '(notmuch-hello-insert-saved-searches
          notmuch-hello-insert-search
          notmuch-hello-insert-recent-searches
          notmuch-hello-insert-alltags))

  (setq notmuch-saved-searches
        `((:name "inbox" :query "tag:inbox" :key "i")
          (:name "unread" :query "tag:unread" :key "u")
          (:name "list" :query "tag:list" :key "l")
          (:name "flagged" :query "tag:flagged" :key "f")
          (:name "git"
           :query "(from:*@github.com)"
           :key ,(kbd "g"))
          (:name "emacs" :query "tag:emacs" :key ,(kbd "e a"))
          (:name "emacs-devel"
           :query "(from:emacs-devel@gnu.org or to:emacs-devel@gnu.org)"
           :sort-order newest-first
           :key ,(kbd "e d"))
          (:name "emacs-orgmode"
           :query "(from:emacs-orgmode@gnu.org or to:emacs-orgmode@gnu.org)"
           :sort-order newest-first
           :key ,(kbd "e o"))
          (:name "lispworks"
           :query "from:lisp-hug@lispworks.com or to:lisp-hug@lispworks.com"
           :key ,(kbd "w"))
          (:name "sent" :query "tag:sent" :key "t")
          (:name "drafts" :query "tag:draft" :key "d")
          (:name "all" :query "*" :key "a")))

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

(defun c/mail-sync ()
  (interactive)
  (async-shell-command "mbsync -a"))

(provide 'chip-email)

;;; chip-email.el ends here
