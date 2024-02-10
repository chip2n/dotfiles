;;; chip-bookmark.el -*- lexical-binding: t -*-

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

(require 'cl-lib)

(use-package bookmark+
  :straight nil
  :demand t
  :load-path "~/.emacs.d/src/bookmark-plus/"
  :bind (("C-c w p" . pin/add)
         ("M-1" . pin/jump-1)
         ("M-2" . pin/jump-2)
         ("M-3" . pin/jump-3)
         ("M-4" . pin/jump-4))
  :init
  ;; Download bookmark-plus from emacs wiki if it doesn't already exist
  (let ((bookmarkplus-dir "~/.emacs.d/src/bookmark-plus/")
        (emacswiki-base "https://www.emacswiki.org/emacs/download/")
        (bookmark-files '("bookmark+.el" "bookmark+-mac.el" "bookmark+-bmu.el" "bookmark+-key.el" "bookmark+-lit.el" "bookmark+-1.el")))
    (require 'url)
    (add-to-list 'load-path bookmarkplus-dir)
    (make-directory bookmarkplus-dir t)
    (mapcar (lambda (arg)
              (let ((local-file (concat bookmarkplus-dir arg)))
                (unless (file-exists-p local-file)
                  (url-copy-file (concat emacswiki-base arg) local-file t))))
            bookmark-files))
  :config
  ;; Skip saving the bookmark position so that we can use bookmarks without
  ;; any stored positions
  (setq bmkp-save-new-location-flag nil))

;;;; Pin

;; This is a simple clone of Harpoon, using bookmark+ functionality

(defun pin/add (num)
  (interactive "cNumber:")
  (assert (cl-digit-char-p num))
  (setf num (string-to-number (char-to-string num)))
  (let ((name (format "<pin> %s" (buffer-name (current-buffer)))))
    (pin/bookmark-set-no-position name)
	(cl-loop for bookmark in bookmark-alist
             for v = (bmkp-get-tag-value bookmark "pin")
             when (and v (= v num))
             do (bookmark-delete bookmark))
    (let ((bookmark (bmkp-get-bookmark name)))
      (bmkp-make-bookmark-temporary bookmark)
      (bmkp-add-tags bookmark (list "pin"))
      (bmkp-set-tag-value bookmark "pin" num))))

(defun pin/jump-to (num)
  (cl-loop for bookmark in bookmark-alist
           for v = (bmkp-get-tag-value bookmark "pin")
           when (and v (= v num))
           return (bookmark-jump bookmark)))

(defun pin/jump-1 ()
  (interactive)
  (pin/jump-to 1))

(defun pin/jump-2 ()
  (interactive)
  (pin/jump-to 2))

(defun pin/jump-3 ()
  (interactive)
  (pin/jump-to 3))

(defun pin/jump-4 ()
  (interactive)
  (pin/jump-to 4))

(defun pin/bookmark-set-no-position (&optional name parg interactivep no-refresh-p)
  (interactive (list nil current-prefix-arg t))
  (save-excursion
    (goto-char (point-min))
    (bookmark-set name parg interactivep no-refresh-p)
    (bookmark-set-position (car bookmark-alist) nil)
    (bookmark-set-front-context-string (car bookmark-alist) nil)
    (bookmark-set-rear-context-string (car bookmark-alist) nil)))

(provide 'chip-bookmark)

;;; chip-bookmark.el ends here
