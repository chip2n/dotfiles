(provide 'init-org)

;; set org todo keywords
(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "WAIT(w@/!)" "|" "DONE(d)" "KILL(c@)")))

;; add timestamp to completed todos
(setq org-log-done 'time)

;; prevent indentation after sections
(setq org-adapt-indentation nil)

;; prevent org source blocks from being indented
(setq org-edit-src-content-indentation 0)

;; set org tag column
(setq org-tags-column -80)

;; resize image according to ATTR_ORG if available
(setq org-image-actual-width nil)

;; add automatic newlines when lines get too long
;; using this instead of word-wrap since it doesn't affect tables
(add-hook 'org-mode-hook (lambda ()
                           (auto-fill-mode)
                           (setq fill-column 80)))

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

;; remove clock entry if total time span is less than one minute
(setq org-clock-out-remove-zero-time-clocks t)

;;; configure refile ------------------------------------------------------------

(setq org-refile-targets '((nil :maxlevel . 3)
                           (org-agenda-files :maxlevel . 3)))
(setq org-refile-use-outline-path 'file)
(setq org-outline-path-complete-in-steps nil)

(defun chip/verify-refile-target ()
  "Exclude todo keywords with a done state from refile targets"
  (not (member (nth 2 (org-heading-components)) org-done-keywords)))

(setq org-refile-target-verify-function 'chip/verify-refile-target)

;;; -----------------------------------------------------------------------------

;;; clock report ----------------------------------------------------------------

;; set default clock report parameters
(setq org-clock-clocktable-default-properties
      '(:scope agenda :maxlevel 2 :block today :fileskip0 t :compact t))

(defun org-clock-report-today ()
  "Insert clock report for today's date."
  (let* ((today (shell-command-to-string "echo -n $(date +%Y-%m-%d)"))
         (org-clock-clocktable-default-properties
          `(:scope agenda :maxlevel 2 :block ,(make-symbol today) :fileskip0 t :compact t)))
    (org-clock-report)))

;; change look of indentation in clocktables
(defun my-org-clocktable-indent-string (level)
  (if (= level 1)
      ""
    (let ((str "╰"))
      (while (> level 2)
        (setq level (1- level)
              str (concat str "──")))
      (concat str "─> "))))
(advice-add 'org-clocktable-indent-string :override #'my-org-clocktable-indent-string)

;;; -----------------------------------------------------------------------------

(defun get-presentation-path ()
  "Prompt for presentation name via minibuffer and return path."
  (let ((name (read-from-minibuffer "Presentation name: "))
        (date (shell-command-to-string "echo -n $(date +%Y%m%d)")))
    (format "~/org/remente/presentations/%s-%s/presentation.org" date name)))

(add-hook 'org-capture-mode-hook 'evil-insert-state)

;; set org templates
(setq org-capture-templates
  `(("t" "TODO" entry (file "~/org/personal/refile.org")
     "* TODO %?")
    ("j" "Journal")
    ("je" "Entry" entry (file+olp+datetree "~/org/personal/journal.gpg")
     "* %?\n%T")
    ("js" "Day summary" entry (file+olp+datetree "~/org/personal/journal.gpg")
     "* Day summary :summary:\n%T\n%?\n\n%(org-clock-report-today)")
    ("ju" "Supplements" entry (file+olp+datetree "~/org/personal/journal.gpg")
     "* Supplements :supplements:\n%T\n| %? |  |")
    ("i" "Idea" entry (file+olp "~/org/personal/ideas.org" "Ideas")
     "* %?" :prepend t)
    ("p" "Remente presentation" entry (function ,(lambda () (find-file (get-presentation-path))))
     "
#+OPTIONS: num:nil
#+OPTIONS: toc:nil
#+OPTIONS: reveal_title_slide:nil
#+REVEAL_EXTRA_CSS: /home/chip/.emacs.d/presentation.css
#+REVEAL_TRANS: linear
#+REVEAL_THEME: solarized
#+REVEAL_HLEVEL: 2

* %?")
    ("m" "Meeting" entry (file "~/org/personal/refile.org")
     "* DONE Meeting with %? :meeting:\n%U" :clock-in t :clock-resume t)))




;; add org directory (allows searching for todos and scheduling items)
(setq org-agenda-files (list "~/org/personal/personal.org"
                             "~/org/personal/refile.org"
                             "~/org/remente/remente.org"
                             ))

;; keep agenda filters after closing agenda buffer
(setq org-agenda-persistent-filter t)

;; prevent org-agenda from destroying splits
(setq org-agenda-window-setup 'current-window)

;; always start agenda on current day instead of mondays
(setq org-agenda-start-on-weekday nil)

;; show only today as default
(setq org-agenda-span 'day)

(defun bh/is-project-p ()
  "Any task with a todo keyword subtask"
  (save-restriction
    (widen)
    (let ((has-subtask)
          (subtree-end (save-excursion (org-end-of-subtree t)))
          (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
      (save-excursion
        (forward-line 1)
        (while (and (not has-subtask)
                    (< (point) subtree-end)
                    (re-search-forward "^\*+ " subtree-end t))
          (when (member (org-get-todo-state) org-todo-keywords-1)
            (setq has-subtask t))))
      (and is-a-task has-subtask))))

(defun bh/skip-non-stuck-projects ()
  "Skip trees that are not stuck projects"
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (if (bh/is-project-p)
          (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
                 (has-next ))
            (save-excursion
              (forward-line 1)
              (while (and (not has-next) (< (point) subtree-end) (re-search-forward "^\\*+ NEXT " subtree-end t))
                (unless (member "WAIT" (org-get-tags-at))
                  (setq has-next t))))
            (if has-next
                next-headline
              nil)) ; a stuck project, has subtasks but no next task
        next-headline))))

(defun bh/is-project-subtree-p ()
  "Any task with a todo keyword that is in a project subtree.
Callers of this function already widen the buffer view."
  (let ((task (save-excursion (org-back-to-heading 'invisible-ok)
                              (point))))
    (save-excursion
      (bh/find-project-task)
      (if (equal (point) task)
          nil
        t))))

(defun bh/find-project-task ()
  "Move point to the parent (project) task if any"
  (save-restriction
    (widen)
    (let ((parent-task (save-excursion (org-back-to-heading 'invisible-ok) (point))))
      (while (org-up-heading-safe)
        (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
          (setq parent-task (point))))
      (goto-char parent-task)
      parent-task)))


(defun bh/is-task-p ()
  "Any task with a todo keyword and no subtask"
  (save-restriction
    (widen)
    (let ((has-subtask)
          (subtree-end (save-excursion (org-end-of-subtree t)))
          (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
      (save-excursion
        (forward-line 1)
        (while (and (not has-subtask)
                    (< (point) subtree-end)
                    (re-search-forward "^\*+ " subtree-end t))
          (when (member (org-get-todo-state) org-todo-keywords-1)
            (setq has-subtask t))))
      (and is-a-task (not has-subtask)))))

(defun bh/skip-non-projects ()
  "Skip trees that are not projects"
  (if (save-excursion (bh/skip-non-stuck-projects))
      (save-restriction
        (widen)
        (let ((subtree-end (save-excursion (org-end-of-subtree t))))
          (cond
           ((bh/is-project-p)
            nil)
           ((and (bh/is-project-subtree-p) (not (bh/is-task-p)))
            nil)
           (t
            subtree-end))))
    (save-excursion (org-end-of-subtree t))))

(defun bh/skip-projects-and-habits-and-single-tasks ()
  "Skip trees that are projects, tasks that are habits, single non-project tasks"
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (cond
       ((member "WAIT" (org-get-tags-at))
        next-headline)
       ((bh/is-project-p)
        next-headline)
       ((and (bh/is-task-p) (not (bh/is-project-subtree-p)))
        next-headline)
       (t
        nil)))))

(defun bh/skip-project-tasks ()
  "Show non-project tasks.
Skip project and sub-project tasks, habits, and project related tasks."
  (save-restriction
    (widen)
    (let* ((subtree-end (save-excursion (org-end-of-subtree t))))
      (cond
       ((bh/is-project-p)
        subtree-end)
       ((bh/is-project-subtree-p)
        subtree-end)
       (t
        nil)))))

(defun bh/skip-non-project-tasks ()
  "Show project tasks.
Skip project and sub-project tasks, habits, and loose non-project tasks."
  (save-restriction
    (widen)
    (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
           (next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (cond
       ((bh/is-project-p)
        next-headline)
       ((and (bh/is-project-subtree-p)
             (member (org-get-todo-state) (list "NEXT")))
        subtree-end)
       ((not (bh/is-project-subtree-p))
        subtree-end)
       (t
        nil)))))

(setq org-agenda-custom-commands
      '(("c" "Unscheduled TODO"
         ((agenda "")
          (tags "refile"
                ((org-agenda-overriding-header "\nRefileable Tasks")
                 (org-tags-match-list-sublevels nil)))
          (tags-todo "-KILL/!"
                     ((org-agenda-overriding-header "Stuck Projects")
                      (org-agenda-skip-function 'bh/skip-non-stuck-projects)
                      (org-agenda-sorting-strategy '(category-keep))))
          (tags-todo "-WAIT-KILL/!"
                     ((org-agenda-overriding-header "Active Projects")
                      (org-agenda-skip-function 'bh/skip-non-projects)
                      (org-agenda-sorting-strategy '(category-keep))))
          (tags-todo "-KILL/!NEXT"
                     ((org-agenda-overriding-header "Next Tasks")
                      (org-agenda-skip-function 'bh/skip-projects-and-habits-and-single-tasks)
                      (org-tags-match-list-sublevels t)
                      (org-agenda-todo-ignore-scheduled t)
                      (org-agenda-todo-ignore-deadlines t)
                      (org-agenda-todo-ignore-with-date t)
                      (org-agenda-sorting-strategy '(todo-state-down effort-up category-keep))))
          (tags-todo "-refile-KILL-WAIT/!"
                     ((org-agenda-overriding-header "Project Subtasks")
                      (org-agenda-skip-function 'bh/skip-non-project-tasks)
                      (org-agenda-todo-ignore-scheduled t)
                      (org-agenda-todo-ignore-deadlines t)
                      (org-agenda-todo-ignore-with-date t)
                      (org-agenda-sorting-strategy '(category-keep))))
          (tags-todo "-refile"
                ((org-agenda-overriding-header "\nUnscheduled")
                 (org-agenda-skip-function 'bh/skip-project-tasks)
                 (org-agenda-tags-todo-honor-ignore-options t)
                 (org-agenda-todo-ignore-scheduled t)
                 (org-agenda-todo-ignore-deadlines t)
                 (org-agenda-todo-ignore-with-date t)
                 )))
         nil
         nil)))

;; hide separators between agenda blocks
(setq org-agenda-block-separator nil)

;; finalize agenda entries (removing icebox tasks)
(defun chip/org-agenda-finalize-entries (string)
  (let ((lines (split-string string "\n" t)))
  (mapconcat 'identity
             (remove-icebox-tasks lines)
             "\n")))

(defun remove-icebox-tasks (lines)
  (remove-if (lambda (line) (string-match-p ":icebox:" line)) lines))

(advice-add 'org-agenda-finalize-entries :filter-return #'chip/org-agenda-finalize-entries)


(defvar chip/org-agenda-scheduled-icon (all-the-icons-material "event" :v-adjust -0.1))
(defvar chip/org-agenda-deadline-icon (all-the-icons-material "whatshot" :v-adjust -0.1))
(setq org-agenda-scheduled-leaders `(,(concat chip/org-agenda-scheduled-icon "") ,(concat chip/org-agenda-scheduled-icon "x%1d")))
(setq org-agenda-deadline-leaders `(,(concat chip/org-agenda-deadline-icon "")
                                    ,(concat chip/org-agenda-deadline-icon "+%1d")
                                    ,(concat chip/org-agenda-deadline-icon "-%1d")))

(setq org-file-apps
         '(("\\.png\\'" . "feh --scale-down \"%s\"")
           ("\\.jpg\\'" . "feh --scale-down \"%s\"")
           (auto-mode . emacs)
           ("\\.mm\\'" . default)
           ("\\.x?html?\\'" . default)
           ("\\.pdf\\'" . default)))
(setq org-startup-indented t)

;; Cleanup intermediate files after org export
(setq org-latex-logfiles-extensions '("tex" "spl"))

(use-package ob-restclient
  :ensure t)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)
   (shell . t)
   (js . t)
   (lilypond . t)
   (ditaa . t)
   (restclient . t)))

;; Enable noweb expansion in all languages
(setq org-babel-default-header-args
      (cons '(:noweb . "yes")
            (assq-delete-all :noweb org-babel-default-header-args)))

(defun my-org-confirm-babel-evaluate (lang body)
  (not (member lang '("python" "bash" "js" "lisp" "lilypond" "ditaa" "restclient"))))

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

(defun enable-dnd ()
  (interactive)
  (autoremote-send "enable-dnd"))

(defun disable-dnd ()
  (interactive)
  (autoremote-send "disable-dnd"))

(use-package org-pomodoro
  :ensure t
  :commands (org-pomodoro)
  :config
  (setq alert-user-configuration (quote ((((:category . "org-pomodoro")) libnotify nil))))
  (add-hook 'org-pomodoro-started-hook 'enable-dnd)
  (add-hook 'org-pomodoro-finished-hook 'disable-dnd)
  (add-hook 'org-pomodoro-killed-hook 'disable-dnd)
  (setq org-pomodoro-short-break-sound "~/audio/waterdrop.wav")
  (setq org-pomodoro-long-break-sound "~/audio/waterdrop.wav")
  (setq org-pomodoro-finished-sound "~/audio/waterdrop.wav")
  (setq org-pomodoro-killed-sound "~/audio/waterdrop.wav")
  (setq org-pomodoro-overtime-sound "~/audio/waterdrop.wav"))

;; package for writing org notes while reading pdf
(use-package org-noter
  :ensure t)

(use-package org-super-agenda
  :ensure t
  :config
  (setq org-super-agenda-header-separator "")
  (setq org-super-agenda-unmatched-name "")
  (setq org-super-agenda-groups
        '((:name ""
                 :time-grid t
                 :todo "TODAY")
          (:name ""
                 :category "remente")
          ))
  (org-super-agenda-mode))


;; stolen from: https://writequit.org/articles/emacs-org-mode-generate-ids.html#automating-id-creation
(require 'org-id)
(setq org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)



(use-package ox-pandoc
  :ensure t)
(setq org-pandoc-options-for-latex-pdf '((pdf-engine . "xelatex")))

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
