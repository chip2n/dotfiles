;;; chip-export.el -*- lexical-binding: t -*-

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

;; We need to use -shell-escape when embedding svg images
(setq org-latex-pdf-process
      '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "bibtex %b"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

(setq org-html-validation-link nil)

(defun c/org-export-html ()
  (interactive)
  (org-html-export-to-html))

(defun c/toggle-org-export-html-on-save ()
  (interactive)
  (if (memq 'c/org-export-html after-save-hook)
      (progn
        (remove-hook 'after-save-hook 'c/org-export-html t)
        (message "Disabled org html export on save for current buffer..."))
    (add-hook 'after-save-hook 'c/org-export-html nil t)
    (message "Enabled org html export on save for current buffer...")))

(defun c/toggle-org-export-on-save (exporter)
  (if (memq exporter after-save-hook)
      (progn
        (remove-hook 'after-save-hook exporter t)
        (message "Disabled org export on save for current buffer..."))
    (add-hook 'after-save-hook exporter nil t)
    (message "Enabled org export on save for current buffer...")))

(provide 'chip-export)

;;; chip-export.el ends here
