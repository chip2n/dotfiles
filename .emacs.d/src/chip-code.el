;;; chip-code.el -*- lexical-binding: t -*-

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

;;; Mode

(define-minor-mode c/code-mode
  "Mode for coding the codes."
  :lighter nil
  :keymap `((,(kbd "M-n") . c/next-error)
            (,(kbd "M-p") . c/prev-error)
            (,(kbd "C-c C-f") . c/format-buffer)))

(defun c/format-buffer ()
  (interactive)
  (lsp-format-buffer))

;;; Compilation buffer

(setq compilation-window-height 20)

(defun c/compilation-hook ()
  "Force compilation buffer to be shown using vertical split with a short height."
  (when (not (get-buffer-window "*compilation*"))
    (save-selected-window
      (save-excursion
        (let* ((w (split-window-vertically))
               (h (window-height w)))
          (select-window w)
          (switch-to-buffer "*compilation*")
          (shrink-window (- h compilation-window-height)))))))
(add-hook 'compilation-mode-hook 'c/compilation-hook)

;;; Compile on save

;; Stolen from: https://rtime.ciirc.cvut.cz/~sojka/blog/compile-on-save/

(defun compile-on-save-start ()
  (let ((buffer (compilation-find-buffer)))
    (unless (get-buffer-process buffer)
      (recompile))))

(define-minor-mode compile-on-save-mode
  "Minor mode to automatically call `recompile' whenever the
current buffer is saved. When there is ongoing compilation,
nothing happens."
  :global nil
  (if compile-on-save-mode
      (progn (make-local-variable 'after-save-hook)
             (add-hook 'after-save-hook 'compile-on-save-start nil t))
    (kill-local-variable 'after-save-hook)))

;;; Compilation input

;; Allows input to be sent to compilation buffers
;; Stolen from: https://endlessparentheses.com/provide-input-to-the-compilation-buffer.html

(defun c/compilation-send-input (input &optional nl)
  "Send INPUT to the current process.
Interactively also sends a terminating newline."
  (interactive "MInput: \nd")
  (let ((string (concat input (if nl "\n"))))
    ;; This is just for visual feedback.
    (let ((inhibit-read-only t))
      (insert-before-markers string))
    ;; This is the important part.
    (process-send-string
     (get-buffer-process (current-buffer))
     string)))

(after-load (general)
  (general-define-key
   :keymaps '(compilation-mode-map)
   "i" 'c/compilation-send-input))

(provide 'chip-code)

;;; Error navigation

(defun c/next-error ()
  (interactive)
  (if (flycheck-next-error-pos 1 t)
      (flycheck-next-error)
    (next-error)))

(defun c/prev-error ()
  (interactive)
  (if (flycheck-next-error-pos 1 t)
      (flycheck-previous-error)
    (previous-error)))

;;; chip-code.el ends here
