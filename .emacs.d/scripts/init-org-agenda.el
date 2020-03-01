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

;; remove agenda indentation
(setq org-agenda-prefix-format
      '((agenda . "%i%-12:c%?-12t% s")
        (todo . "%i%-12:c")
        (tags . "%i%-12:c")
        (search . "%i%-12:c")))

;; make time grid as wide as the tag column
(setq org-agenda-time-grid
      '((daily today require-timed)
        (800 1000 1200 1400 1600 1800 2000)
        "......" "--------------------------------------------------------"))
(setq org-agenda-current-time-string
      "--------------------------------------------------------")

;; force child TODOs to be done before parent can be done
(setq org-enforce-todo-dependencies t)

;; remove header
(setq org-agenda-overriding-header "")

;; format dates in a nicer way
(setq org-agenda-format-date 'chip/org-agenda-format-date-aligned)

(defun chip/org-agenda-format-date-aligned (date)
  "Format a DATE string for display in the daily/weekly agenda.
This function makes sure that dates are aligned for easy reading."
  (require 'cal-iso)
  (let* ((dayname (downcase (calendar-day-name date)))
	 (day (cadr date))
	 (day-of-week (calendar-day-of-week date))
	 (month (car date))
	 (monthname (downcase (calendar-month-name month)))
	 (year (nth 2 date))
	 (iso-week (org-days-to-iso-week
		    (calendar-absolute-from-gregorian date)))
	 (weekyear (cond ((and (= month 1) (>= iso-week 52))
			  (1- year))
			 ((and (= month 12) (<= iso-week 1))
			  (1+ year))
			 (t year)))
	 (weekstring (if (= day-of-week 1)
			 (format " w%02d" iso-week)
		       "")))
    (let* ((lhs dayname)
           (rhs (format "%2d %s %4d" day monthname year))
           (padding (- 80 (length lhs) (length rhs) 2))
           (pad-str (make-string padding ?—))
           (pattern (format "%%s%%%ds" padding)))
      (format "%s %s %s" lhs pad-str rhs))))

;; set agenda icons
(setq org-agenda-scheduled-leaders `("" "(+%1d)"))
(setq org-agenda-deadline-leaders `("(!!)" "(-%1d)" "(+%1d)"))

(setq org-todo-state-tags-triggers
      (quote (("TODO" ("WAIT" . nil)
                      ("KILL" . nil)
                      ("HOLD" . nil))
              ("NEXT" ("WAIT" . nil)
                      ("KILL" . nil)
                      ("HOLD" . nil))
              ("WAIT" ("HOLD" . nil)
                      ("WAIT" . t))
              ("HOLD" ("WAIT" . nil)
                      ("HOLD" . t))
              ("KILL" ("KILL" . t))
              ("DONE" ("WAIT" . nil)
                      ("KILL" . nil)
                      ("HOLD" . nil))
              (done   ("WAIT" . nil)
                      ("HOLD" . nil)))))

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
                 (has-next nil))
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

(defun bh/skip-projects-and-subtasks ()
  "Skip trees that are projects or project subtasks"
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (cond
       ((member "WAIT" (org-get-tags-at))
        next-headline)
       ((bh/is-project-p)
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

(defun bh/skip-projects ()
  "Show tasks and project subtasks."
  (save-restriction
    (widen)
    (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
           (next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (cond
       ((bh/is-project-p)
        ;; don't show subtasks if project is WAIT
        (if (member (org-get-todo-state) (list "WAIT"))
            subtree-end
            next-headline))
       ((member (org-get-todo-state) (list "NEXT"))
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
          (tags-todo "-HOLD-KILL/!NEXT"
                     ((org-agenda-overriding-header "\nnext ———————————————————————————————————————————————————————————————————————————")
                      ;;; TODO don's skip single tasks
                      (org-agenda-skip-function 'bh/skip-projects-and-subtasks)
                      (org-tags-match-list-sublevels t)
                      (org-agenda-todo-ignore-scheduled t)
                      (org-agenda-todo-ignore-deadlines t)
                      (org-agenda-todo-ignore-with-date t)
                      (org-agenda-sorting-strategy '(todo-state-down effort-up category-keep))))
          (tags-todo "-HOLD-WAIT-KILL/!"
                     ((org-agenda-overriding-header "projects ———————————————————————————————————————————————————————————————————————")
                      (org-agenda-skip-function 'bh/skip-non-projects)
                      (org-agenda-sorting-strategy '(category-keep))))
          (tags-todo "-KILL-HOLD/!"
                     ((org-agenda-overriding-header "inactive ———————————————————————————————————————————————————————————————————————")
                      (org-agenda-skip-function 'bh/skip-non-stuck-projects)
                      (org-agenda-sorting-strategy '(category-keep))))
          (tags-todo "-refile-KILL-HOLD/!"
                     ((org-agenda-overriding-header "tasks ——————————————————————————————————————————————————————————————————————————")
                      (org-agenda-skip-function 'bh/skip-projects)
                      (org-agenda-tags-todo-honor-ignore-options t)
                      (org-agenda-todo-ignore-scheduled t)
                      (org-agenda-todo-ignore-deadlines t)
                      (org-agenda-todo-ignore-with-date t)
                      (org-agenda-sorting-strategy '(category-keep))))
          (tags "refile"
                ((org-agenda-overriding-header "\nrefile —————————————————————————————————————————————————————————————————————————")
                 (org-tags-match-list-sublevels nil)))
          (tags "-refile/"
                ((org-agenda-overriding-header "archive ————————————————————————————————————————————————————————————————————————")
                 (org-agenda-skip-function 'bh/skip-non-archivable-tasks)
                 (org-tags-match-list-sublevels nil))))
         nil
         nil)))

(provide 'init-org-agenda)
