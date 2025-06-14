;;; chip-code-nim.el -*- lexical-binding: t -*-

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

(defun c/nim--setup ()
  ;; Make files in the nimble folder read only by default.
  ;; This can prevent to edit them by accident.
  (when (string-match "/\.nimble/" (or (buffer-file-name) "")) (read-only-mode 1))

  ;; (local-set-key (kbd "M->") 'nim-indent-shift-right)
  ;; (local-set-key (kbd "M-<") 'nim-indent-shift-left)
  (electric-indent-local-mode 0)
  (auto-fill-mode 0)
  (indent-guide-mode 1))

(defun c/locate-dominating-file-rx (rx)
  "Like `locate-dominating-file', but searches using a regex."
  (cl-flet ((correct-dir? ()
                         (let ((files (directory-files default-directory)))
                           (seq-some (lambda (file) (string-match-p rx file)) files))))
    (with-temp-buffer
      (while (and (not (correct-dir?)) (not (equal "/" default-directory)))
        (cd ".."))
      (when (correct-dir?)
        default-directory))))

(defun c/nim-compile ()
  (interactive)
  (when-let ((default-directory (c/locate-dominating-file-rx "\\.nimble$")))
    (call-interactively 'compile)))

(defun c/nim-recompile ()
  (interactive)
  (when-let ((default-directory (c/locate-dominating-file-rx "\\.nimble$")))
    (call-interactively 'recompile)))

(use-package nim-mode
  :hook ((nim-mode . c/nim--setup))
  :bind (:map nim-mode-map
         ("C-c C-c" . c/smart-compile)
         ("C-c C-r" . c/smart-recompile)
         :map nimscript-mode-map
         ("C-c C-c" . c/smart-compile)
         ("C-c C-r" . c/smart-recompile)))

(use-package indent-guide
  :config
  (setq indent-guide-char "│")
  (setq indent-guide-recursive t))

(provide 'chip-code-nim)

;;; chip-code-nim.el ends here
