(require 'olivetti)
(require 'org-bullets)

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

;; TODO rename
(defun prose--remove-stars ()
  (font-lock-add-keywords
   nil                         ; highlights added for the current buffer
   '(("^\\*\\*+ "              ; match every headline except the first
      (0
       (prog1 nil
         (put-text-property (match-beginning 0)
                            (- (match-end 0) 2)
                            'invisible t)))))))

(defun prose--finish ()
  (interactive)
  (clipboard-kill-ring-save (point-min) (point-max))
  (message "Prose saved to kill ring.")
  (bury-buffer))

(defun prose--clear ()
  (interactive)
  (kill-region (point-min) (point-max))
  (message "Prose cleared."))

(define-minor-mode prose-mode
  "Mode for writing prose."
  :global nil
  :keymap '(("\C-c\C-c" . prose--finish)
            ("\C-c\C-k" . prose--clear)))

(defun prose ()
  (interactive)
  (let ((buffer (get-buffer-create "*prose*")))
    (switch-to-buffer buffer)

    (org-mode)
    (olivetti-mode)
    (prose-mode)

    (auto-fill-mode -1)
    (org-indent-mode -1)
    (org-bullets-mode 1)

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

    (prose--remove-stars)))

(provide 'chip-prose)
