(provide 'init-org)

;; add support for block expansions (e.g. <s)
(require 'org-tempo)

;; add timestamp to completed todos
(setq org-log-done 'time)

;; prevent indentation after sections
(setq org-adapt-indentation nil)

;; add org directory (allows searching for todos and scheduling items)
(setq org-agenda-files (list "~/org/personal" "~/org/remente"))
(setq org-refile-targets '((org-agenda-files :maxlevel . 3)))

;; set org tag column
(setq org-tags-column -80)

;; resize image according to ATTR_ORG if available
(setq org-image-actual-width nil)

;; prevent org-agenda from destroying splits
(setq org-agenda-window-setup 'current-window)

;; always start agenda on current day instead of mondays
(setq org-agenda-start-on-weekday nil)

;; add automatic newlines when lines get too long
;; using this instead of word-wrap since it doesn't affect tables
(add-hook 'org-mode-hook (lambda () (auto-fill-mode)))

;; auto-saving org buffers after certain actions
(defun save-org-buffers (&rest args)
  (org-save-all-org-buffers))
(advice-add 'org-agenda-quit :before 'save-org-buffers)
(advice-add 'org-deadline :after 'save-org-buffers)
(advice-add 'org-refile :after 'save-org-buffers)
(advice-add 'org-schedule :after 'save-org-buffers)
(advice-add 'org-set-tags-command :after 'save-org-buffers)
(advice-add 'org-clock-in :after 'save-org-buffers)
(advice-add 'org-clock-out :after 'save-org-buffers)
(advice-add 'org-todo :after 'save-org-buffers)

;; open links in same window
;(setq org-link-frame-setup (file . find-file))

;; set org templates
(setq org-capture-templates
  `(("t" "Personal TODO" entry (file+olp "~/org/personal/todos.org" "Tasks")
     "* TODO %?" :prepend t)
    ("j" "Journal" entry (file+olp+datetree "~/org/personal/journal.gpg") "* %?\n%T")
    ("i" "Idea" entry (file+olp "~/org/personal/ideas.org" "Ideas")
     "* %?" :prepend t)
    ("r" "Remente TODO" entry (file+olp "~/org/remente/notes.org" "Tasks")
     "* TODO %?" :prepend t)))

(setq org-agenda-custom-commands
      '(("c" "Unscheduled TODO"
         ((agenda "")
          (todo ""
                ((org-agenda-overriding-header "\nUnscheduled")
                 (org-agenda-skip-function '(org-agenda-skip-entry-if 'timestamp)))))
         nil
         nil)))
(setq org-agenda-block-separator ?―)

;; set org todo keywords
(setq org-todo-keywords
      '((sequence "TODO(t)" "WAITING(w)" "|" "DONE(d)" "CANCELED(c@)")))

(setq org-file-apps
         '(("\\.png\\'" . "feh --scale-down \"%s\"")
           ("\\.jpg\\'" . "feh --scale-down \"%s\"")
           (auto-mode . emacs)
           ("\\.mm\\'" . default)
           ("\\.x?html?\\'" . default)
           ("\\.pdf\\'" . default)))
(setq org-ellipsis " ")
(setq org-startup-indented t)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t) (shell . t) (js . t)))
(defun my-org-confirm-babel-evaluate (lang body)
  (not (member lang '("python" "bash" "js"))))

(setq org-confirm-babel-evaluate 'my-org-confirm-babel-evaluate)

;; Redisplay inlined images after source block execution
(add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images)

(require 'org-drill)

;; (use-package evil-org
;;    :ensure t
;;    :after org
;;    :config
;;    (add-hook 'org-mode-hook 'evil-org-mode)
;;    (add-hook 'evil-org-mode-hook
;;              (lambda ()
;;                (evil-org-set-key-theme))))


(use-package org-bullets
  :ensure t
  :after org
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
  (setq org-bullets-bullet-list '("")))

(require 'private)
(use-package org-gcal
  :ensure t
  :after org
  :config
  (setq org-gcal-client-id private/gcal-client-id
        org-gcal-client-secret private/gcal-client-secret
        org-gcal-file-alist `((,private/gcal-calendar-id . "~/org/personal/gcal.org"))))

(use-package ob-http
  :ensure t
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (lisp . t)
     (http . t))))

(use-package org-pomodoro
  :ensure t
  :commands (org-pomodoro)
  :config
  (setq alert-user-configuration (quote ((((:category . "org-pomodoro")) libnotify nil)))))

;; package for writing org notes while reading pdf
(use-package org-noter
  :ensure t)


;; (use-package org-super-agenda
;;   :ensure t
;;   :config
;;   (setq org-super-agenda-header-separator "")
;;   (setq org-super-agenda-unmatched-name "Other")
;;   (setq org-super-agenda-groups
;;         '((:name "Schedule"
;;                  :time-grid t
;;                  :todo "TODAY")
;;           (:name "Work"
;;                  :category "remente")
;;           ))
;;   (org-super-agenda-mode))


;; stolen from: https://writequit.org/articles/emacs-org-mode-generate-ids.html#automating-id-creation
(require 'org-id)
(setq org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)

(defun org-create-custom-id ()
  (interactive)
  (chip/org-custom-id-get (point) 'create))

(defun chip/org-custom-id-get (&optional pom create prefix)
  "Get the CUSTOM_ID property of the entry at point-or-marker POM.
   If POM is nil, refer to the entry at point. If the entry does
   not have an CUSTOM_ID, the function returns nil. However, when
   CREATE is non nil, create a CUSTOM_ID if none is present
   already. PREFIX will be passed through to `org-id-new'. In any
   case, the CUSTOM_ID of the entry is returned."
  (interactive)
  (org-with-point-at pom
    (let ((id (org-entry-get nil "CUSTOM_ID")))
      (cond
       ((and id (stringp id) (string-match "\\S-" id))
        id)
       (create
        (setq id (org-id-new (concat prefix "h")))
        (org-entry-put pom "CUSTOM_ID" id)
        (org-id-add-location id (buffer-file-name (buffer-base-buffer)))
        id)))))
