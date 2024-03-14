;;; chip-project.el -*- lexical-binding: t -*-

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

;; This file contains code for working with projects.

;;; Code:

(defvar-local c/project-org-id nil
  "ID for org header associated with current project.
This can be used with directory local variables to be able to jump the project
org task quickly.")

(defun c/project-org-jump ()
  "Jump to org header associated with current project."
  (interactive)
  (unless c/project-org-id (error "Set c/project-org-id variable to the ID of the org header for this project."))
  (org-id-goto c/project-org-id))

(defmacro with-project-root (&rest body)
  "Run `body' in the root of the project as located by projectile."
  (let ((project-root-buffer (gensym)))
    `(save-excursion
       (let ((,project-root-buffer (find-file-noselect (projectile-project-root))))
         (set-buffer ,project-root-buffer)
         ,@body
         (kill-buffer ,project-root-buffer)))))

(defmacro with-dominating-file-dir (filename &rest body)
  "Run `body' in the root of the project as located by projectile."
  (declare (indent defun))
  (let ((project-root-buffer (gensym)))
    `(save-excursion
       (let ((,project-root-buffer (find-file-noselect (locate-dominating-file default-directory ,filename))))
         (set-buffer ,project-root-buffer)
         ,@body
         (kill-buffer ,project-root-buffer)))))

(use-package project
  :bind (:map project-prefix-map
         ("m" . magit-project-status)
         ("g" . c/grep)
         ("t" . c/project-vterm))
  :bind (("C-c p p" . project-switch-project)
         ("C-c p f" . project-find-file))
  :config
  (setq project-switch-commands
        '((project-find-file "find-file")
          (project-find-dir "find-dir")
          (project-eshell "eshell")
          (magit-project-status "magit")
          (c/project-vterm "vterm")
          (c/grep "grep"))))

(use-package projectile
  :disabled t
  :config
  (c/diminish projectile-mode)

  (add-to-list 'projectile-globally-ignored-directories "*node_modules")
  (setq projectile-enable-caching nil)
  ;; we remove -o flag so that untracked files are not included
  ;; this is mainly so that they don't always appear as the first search
  (setq projectile-git-command "git ls-files -zc --exclude-standard")
  (setq projectile-indexing-method 'alien)
  (projectile-register-project-type 'shadow-cljs '("shadow-cljs.edn")
                                    :src-dir "src/main"
                                    :test-dir "src/test"
                                    :test-suffix "_test")
  (projectile-mode))

(use-package counsel-projectile
  :disabled t
  :after (counsel projectile))

(provide 'chip-project)

;;; chip-project.el ends here
