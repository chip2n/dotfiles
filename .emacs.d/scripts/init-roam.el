(use-package org-roam
  :after org
  :load-path "elisp/org-roam"
  :hook
  ((org-mode . org-roam-mode)
   (after-init . org-roam--build-cache-async))
  :config
  (setq org-roam-directory "/home/chip/org/personal/roam")
  (setq org-roam-buffer-width 0.4)
  (setq org-roam-encrypt-files nil)
  (add-to-list 'evil-emacs-state-modes 'org-roam-backlinks-mode)
  :bind
  ("C-c n l" . org-roam)
  ("C-c n t" . org-roam-today)
  ("C-c n f" . org-roam-find-file)
  ("C-c n i" . org-roam-insert))

(use-package deft
  :ensure t
  :after (org evil)
  :bind
  ("C-c n d" . deft)
  :config
  (setq deft-recursive t)
  (setq deft-use-filter-string-for-filename t)
  (setq deft-default-extension "org")
  (setq deft-directory "/home/chip/org/personal/roam")
  (setq deft-use-filename-as-title nil)
  (setq deft-extensions '("txt" "text" "md" "markdown" "org" "gpg"))
  (add-to-list 'evil-emacs-state-modes 'deft-mode)

  ;; deft matches directory name as well, so we'll fix it by copying the
  ;; deft-filter-match-file function and changing one line.
  ;; see: https://github.com/jrblevin/deft/issues/66
  (defun deft-filter-match-file (file &optional batch)
    "Return FILE if it is a match against the current filter regexp.
If BATCH is non-nil, treat `deft-filter-regexp' as a list and match
all elements."
    (with-temp-buffer
      (insert (file-name-nondirectory file)) ;; only changed this line
      (let ((title (deft-file-title file))
            (contents (if deft-filter-only-filenames "" (deft-file-contents file))))
        (when title (insert title))
        (when contents (insert contents)))
      (if batch
          (if (every (lambda (filter)
                       (goto-char (point-min))
                       (deft-search-forward filter))
                     deft-filter-regexp)
              file)
        (goto-char (point-min))
        (if (deft-search-forward (car deft-filter-regexp))
            file)))))

(provide 'init-roam)
