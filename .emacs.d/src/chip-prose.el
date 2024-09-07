;;; chip-prose.el --- Mode for focused writing  -*- lexical-binding: t -*-

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

(require 'olivetti)

(defgroup chip-prose
  nil
  "")

(defface chip-face-prose nil
  ""
  :group 'chip)

(set-face-attribute 'chip-face-prose nil
                    :foreground "#ff0000"
                    :background nil
                    :height 400)

(defface chip-prose-org-level-1
  `((t :inherit org-level-1
       :height 160))
  ""
  :group 'chip-prose)

(defface chip-prose-org-level-2
  `((t :inherit org-level-2
       :height 130))
  ""
  :group 'chip-prose)

(defface chip-prose-org-level-3
  `((t :inherit org-level-3
       :height 100))
  ""
  :group 'chip-prose)

(defface chip-prose-org-todo
  '((t :inherit org-todo
       ;; :height 300
       ;; :underline t
       ))
  ""
  :group 'chip-prose)

(defface chip-prose-org-done
  '((t :inherit org-done
       ;; :height 150
       ;; :underline t
       ))
  ""
  :group 'chip-prose)

(defface chip-prose-org-document-title
  '((t :inherit org-document-title
       :height 150
       :bold t))
  ""
  :group 'chip-prose)

(defface chip-prose-org-document-info
  '((t :inherit org-document-title
       :height 110
       :bold t))
  ""
  :group 'chip-prose)

(defvar prose-finish-hook nil
  "Hook called when prose is finished.")

;; TODO rename
(defun prose--remove-stars ()
  (font-lock-add-keywords
   nil                       ; highlights added for the current buffer
   '(("^\\*\\*+ "            ; match every headline except the first
      (0
       (prog1 nil
         (put-text-property (match-beginning 0)
                            (- (match-end 0) 2)
                            'invisible t))))))
  ;; refresh buffer in case something is out of date
  (font-lock-flush))

(defun prose--finish ()
  (interactive)
  (save-buffer)
  ;; Not enough to save with clipboard-kill-ring-save if frame is killed
  ;; immediately afterwards, so we use xclip to ensure it works properly in that
  ;; case
  ;; (clipboard-kill-ring-save (point-min) (point-max))
  (call-process "xclip" nil nil nil "-selection" "clipboard" "-rmlastnl" (buffer-file-name))
    (message "Prose saved to kill ring.")
  (bury-buffer)
  (when (frame-parameter (selected-frame) 'prose)
    (delete-frame (selected-frame)))
  (run-hooks 'prose-finish-hook))

(defun prose--clear ()
  (interactive)
  (kill-region (point-min) (point-max))
  (message "Prose cleared."))

(define-minor-mode prose-mode
  "Mode for writing prose."
  :global nil
  :keymap '(("\C-c\C-c" . prose--finish)
            ("\C-c\C-k" . prose--clear)))

(defvar prose--auto-save-timer nil)
(defun prose--enable (buffer)
  (interactive)
  (olivetti-mode)
  (auto-fill-mode -1)
  (org-indent-mode -1)

  ;; TODO add more levels
  (face-remap-add-relative 'org-level-1 'chip-prose-org-level-1)
  (face-remap-add-relative 'org-level-2 'chip-prose-org-level-2)
  (face-remap-add-relative 'org-level-3 'chip-prose-org-level-3)
  (face-remap-add-relative 'org-todo 'chip-prose-org-todo)
  (face-remap-add-relative 'org-done 'chip-prose-org-done)
  (face-remap-add-relative 'org-document-title 'chip-prose-org-document-title)
  (face-remap-add-relative 'org-document-info 'chip-prose-org-document-info)

  ;; hide title / author ... keywords
  (setq-local org-hidden-keywords '(title author date email))
  (prose--remove-stars)

  (prose--start-timer)
  (add-hook 'kill-buffer-hook 'prose--cancel-timer))

(defun prose--start-timer ()
  (unless prose--auto-save-timer
    (message "Enabling timer")
    (setq prose--auto-save-timer
          (run-with-idle-timer 10 t (lambda ()
                                      (when-let ((buffer (get-buffer "*prose*")))
                                        (with-current-buffer buffer
                                          (save-buffer))))))))

(defun prose--cancel-timer ()
  (when (and prose-mode prose--auto-save-timer)
    (cancel-timer prose--auto-save-timer)
    (setq prose--auto-save-timer nil)))

(defun prose ()
  (interactive)
  (let* ((filename (concat user-emacs-directory ".prose"))
         (buffer (find-file filename)))

    (rename-buffer "*prose*")
    (unless prose-mode
      (org-mode)
      (prose-mode)
      (prose--enable buffer)
      (when c/config-evil?
        (evil-emacs-state))

      ;; Goto end of buffer, skipping the last newline
      (goto-char (- (point-max) 1)))))

(provide 'chip-prose)

 ;;; chip-prose.el ends here
