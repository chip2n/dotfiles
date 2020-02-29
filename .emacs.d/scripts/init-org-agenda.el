(setq org-agenda-files
      '("~/org/personal/personal.org"
        "~/org/personal/refile.org"
        "~/org/remente/remente.org"))

(setq org-agenda-tags-column -80)

;; keep agenda filters after closing agenda buffer
(setq org-agenda-persistent-filter t)

;; prevent org-agenda from destroying splits
(setq org-agenda-window-setup 'current-window)

;; always start agenda on current day instead of mondays
(setq org-agenda-start-on-weekday nil)

;; show only today as default
(setq org-agenda-span 'day)

;; hide separators between agenda blocks
(setq org-agenda-block-separator nil)

;; finalize agenda entries (removing icebox tasks)
(defun remove-icebox-tasks (lines)
  (remove-if (lambda (line) (string-match-p ":icebox:" line)) lines))
(defun chip/org-agenda-finalize-entries (string)
  (let ((lines (split-string string "\n" t)))
  (mapconcat 'identity
             (remove-icebox-tasks lines)
             "\n")))
(advice-add 'org-agenda-finalize-entries :filter-return #'chip/org-agenda-finalize-entries)

;; set agenda icons
(defvar chip/org-agenda-scheduled-icon (all-the-icons-material "event" :v-adjust -0.1))
(defvar chip/org-agenda-deadline-icon (all-the-icons-material "whatshot" :v-adjust -0.1))
(setq org-agenda-scheduled-leaders `(,(concat chip/org-agenda-scheduled-icon "") ,(concat chip/org-agenda-scheduled-icon "x%1d")))
(setq org-agenda-deadline-leaders `(,(concat chip/org-agenda-deadline-icon "")
                                    ,(concat chip/org-agenda-deadline-icon "+%1d")
                                    ,(concat chip/org-agenda-deadline-icon "-%1d")))

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

(setq org-todo-state-tags-triggers
      (quote (("KILL" ("KILL" . t))
              ("WAIT" ("WAIT" . t))
              ("HOLD" ("WAIT") ("HOLD" . t))
              (done ("WAIT") ("HOLD"))
              ("TODO" ("WAIT") ("KILL") ("HOLD"))
              ("NEXT" ("WAIT") ("KILL") ("HOLD"))
              ("DONE" ("WAIT") ("KILL") ("HOLD")))))

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

(defun bh/skip-non-archivable-tasks ()
  "Skip trees that are not available for archiving"
  (save-restriction
    (widen)
    ;; Consider only tasks with done todo headings as archivable candidates
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max))))
          (subtree-end (save-excursion (org-end-of-subtree t))))
      (if (member (org-get-todo-state) org-todo-keywords-1)
          (if (member (org-get-todo-state) org-done-keywords)
              (let* ((daynr (string-to-number (format-time-string "%d" (current-time))))
                     (a-month-ago (* 60 60 24 (+ daynr 1)))
                     (last-month (format-time-string "%Y-%m-" (time-subtract (current-time) (seconds-to-time a-month-ago))))
                     (this-month (format-time-string "%Y-%m-" (current-time)))
                     (subtree-is-current (save-excursion
                                           (forward-line 1)
                                           (and (< (point) subtree-end)
                                                (re-search-forward (concat last-month "\\|" this-month) subtree-end t)))))
                (if subtree-is-current
                    subtree-end ; Has a date in this month or last month, skip it
                  nil))  ; available to archive
            (or subtree-end (point-max)))
        next-headline))))

(setq org-agenda-custom-commands
      '(("c" "Unscheduled TODO"
         ((agenda "")
          (tags "refile"
                ((org-agenda-overriding-header "\nRefileable Tasks")
                 (org-tags-match-list-sublevels nil)))
          (tags-todo "-KILL-HOLD/!"
                     ((org-agenda-overriding-header "Stuck Projects")
                      (org-agenda-skip-function 'bh/skip-non-stuck-projects)
                      (org-agenda-sorting-strategy '(category-keep))))
          (tags-todo "-HOLD-WAIT-KILL/!"
                     ((org-agenda-overriding-header "Active Projects")
                      (org-agenda-skip-function 'bh/skip-non-projects)
                      (org-agenda-sorting-strategy '(category-keep))))
          (tags-todo "-HOLD-KILL/!NEXT"
                     ((org-agenda-overriding-header "Next Tasks")
                      (org-agenda-skip-function 'bh/skip-projects-and-habits-and-single-tasks)
                      (org-tags-match-list-sublevels t)
                      (org-agenda-todo-ignore-scheduled t)
                      (org-agenda-todo-ignore-deadlines t)
                      (org-agenda-todo-ignore-with-date t)
                      (org-agenda-sorting-strategy '(todo-state-down effort-up category-keep))))
          (tags-todo "-refile-KILL-WAIT-HOLD/!"
                     ((org-agenda-overriding-header "Project Subtasks")
                      (org-agenda-skip-function 'bh/skip-non-project-tasks)
                      (org-agenda-todo-ignore-scheduled t)
                      (org-agenda-todo-ignore-deadlines t)
                      (org-agenda-todo-ignore-with-date t)
                      (org-agenda-sorting-strategy '(category-keep))))
          (tags "-refile/"
                ((org-agenda-overriding-header "Tasks to Archive")
                 (org-agenda-skip-function 'bh/skip-non-archivable-tasks)
                 (org-tags-match-list-sublevels nil)))
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

(provide 'init-org-agenda)
