(require 'cl-lib)

(defun chip-headerline--format-filepath ()
  (chip-modeline-format
   '(" λ "
     chip-headerline-format
     chip-headerline-tag-buffer-position)
   '()))

(defun chip-headerline--format-buffer ()
  (chip-modeline-format
   '(" λ %b" chip-headerline-tag-buffer-position)
   '()))

(defun chip-headerline--format-active ()
  (cl-case major-mode
    ('treemacs-mode " λ projects")
    ('org-agenda-mode " λ agenda")
    ('eat-mode " λ term")
    ('flutter-mode " λ flutter")
    (t
     (if (bound-and-true-p prose-mode)
         `(:propertize " " face chip-face-prose)
       (if (buffer-file-name)
           (chip-headerline--format-filepath)
         (chip-headerline--format-buffer))))))

(defun chip-headerline--format-discrete ()
  '((:eval (if (buffer-file-name)
               (chip-modeline-format
                '(" λ " chip-headerline-format)
                '())
             (chip-modeline-format
              '(" λ %b")
              '())))))

(setq-default header-line-format
              '((:eval (chip-headerline--format-active))))

(setq-default treemacs-user-header-line-format '((" λ treemacs")))
(setq-default treemacs-user-mode-line-format "")

(defun chip-headerline-format ()
  (let* ((sl/full-header (abbreviate-file-name buffer-file-name))
         (path (file-name-directory sl/full-header))
         (filename (file-name-nondirectory buffer-file-name)))
    (if (> (+ (length sl/full-header) 15)
           (window-body-width))
        filename
        ;; (propertize filename 'face 'chip-face-default)
      (concat path filename)
      ;; (propertize (concat path filename) 'face 'chip-face-default)
      )))

(defun chip-headerline-tag-buffer-position ()
  "Tag used to display where in the file the point is"
  (propertize " : %l,%c" 'face 'chip-face-discrete))

(provide 'chip-headerline)
