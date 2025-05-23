;;; chip-code-gpt.el -*- lexical-binding: t -*-

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

(use-package gptel
  :config
  (setq gptel-api-key private/openai-key)
  (setq-default gptel-model "gpt-4o")
  (setq-default gptel-default-mode 'org-mode)
  (setq-default gptel-track-media t)
  (setq gptel-prompt-prefix-alist
        '((markdown-mode . "# ")
          (org-mode . "* ")
          (text-mode . "# ")))
  (gptel-make-anthropic "Claude" :stream t :key private/anthropic-key))

(use-package aider
  :straight (:host github :repo "tninja/aider.el" :files ("aider.el"))
  :config
  (setq aider-args '("--model" "anthropic/claude-3-7-sonnet-latest"
                     "--no-auto-commits")))

(provide 'chip-gpt)

;;; chip-code-gpt.el ends here
