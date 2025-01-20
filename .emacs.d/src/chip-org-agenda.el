;;; chip-org-agenda.el --- My org-agenda configuration  -*- lexical-binding: t -*-

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

;;; Agenda files

;; I use separate org files for personal and work related tasks.

;; I use a separate file for the inbox (refile.org). Tasks are sent here via M-x
;; org-capture, and are automatically tagged with the refile tag implicitly
;; using the following at the top of the file:
;;
;; #+FILETAGS: refile

(setq org-agenda-files
      `(,(concat c/org-dir "personal/personal.org")
        ,(concat c/org-dir "personal/refile.org")
        ,(concat c/org-dir "remente/remente.org")
        ,(concat c/org-dir "findity/findity.org")))

;;; Launching

(defvar c/org-agenda--todo-keyword-regex
  (concat
   (-reduce (lambda (cur acc)
             (concat acc "\\|" cur))
           (mapcar (lambda (entry) (concat "\\* " entry))
                   '("TODO" "NEXT" "WAIT" "HOLD")))
   "\\|SCHEDULED\\|DEADLINE")
  "Regex which filters all TODO keywords")

(defun org-agenda--calculate-files-for-regex (regex)
  "Yields a fresh array with all files containing todos which match REGEX.

Uses grep to discover all files containing anything stored in
org-agenda--todo-keyword-regex.")

(defun c/org-agenda-files-extra ()
  "Find extra files to include in Agenda (searching for relevant org-roam files)."
  (remove-if #'file-directory-p
   (split-string
    (shell-command-to-string
     (concat "grep --include=\"*.org\" -rl -e '" c/org-agenda--todo-keyword-regex "' " c/org-roam-dir))
    "\n")))

(defun c/org-agenda-files ()
  (append org-agenda-files (c/org-agenda-files-extra)))

(defun c/org-agenda ()
  (interactive)
  (let ((org-agenda-files (c/org-agenda-files)))
    (org-agenda nil "c")))

;;; Task states

;; I use the following keywords for task states:
;; - TODO: Base state for a task that is not done yet.
;; - NEXT: Task is marked as the next one to do. It appears at the top of each agenda block.
;; - WAIT: Task is blocked. It appears in the "wait" block.
;; - HOLD: Task is blocked. It is hidden from the agenda.
;; - DONE: The task is done.
;; - KILL: The task is canceled.

(setq org-todo-keywords
      '((sequence "TODO(t)"
                  "NEXT(n)"
                  "WAIT(w@/!)"
                  "HOLD(h@/!)"
                  "|"
                  "DONE(d)"
                  "KILL(c@)")))

;; Some task states automatically add a tag, which is used by the agenda to put
;; the task in the correct block.

(setq org-todo-state-tags-triggers
      (quote (("KILL" ("KILL" . t))
              ("WAIT" ("WAIT" . t))
              ("HOLD" ("WAIT" . nil) ("HOLD" . t))
              (done   ("WAIT" . nil) ("HOLD" . nil))
              ("TODO" ("WAIT" . nil) ("KILL" . nil) ("HOLD" . nil))
              ("NEXT" ("WAIT" . nil) ("KILL" . nil) ("HOLD" . nil))
              ("DONE" ("WAIT" . nil) ("KILL" . nil) ("HOLD" . nil)))))

;; Set the sort strategy for the agenda. In general, we put NEXT tasks at the
;; top, followed by high-priority items, followed by category.
(setq org-agenda-sorting-strategy
      '((agenda time-up todo-state-down priority-down category-keep)
        (todo todo-state-down priority-down category-keep)
        (tags todo-state-down priority-down category-keep)
        (search todo-state-down priority-down category-keep)))

;;; Behavior

;; Show only today as default
(setq org-agenda-span 'day)

;; Always start agenda on current day instead of mondays
(setq org-agenda-start-on-weekday nil)

;; Prevent agenda from destroying splits by opening it in the current window
(setq org-agenda-window-setup 'current-window)

;; Keep agenda filters after closing agenda buffer
(setq org-agenda-persistent-filter t)

;; Deadlines are discretely displayed in the agenda 7 days before due
;; date, and are extra highlighted 2 days before.
(setq org-deadline-warning-days 7)
(setq org-agenda-deadline-faces
      '((1.0 . org-warning)
        (0.7 . org-upcoming-deadline)
        (0.0 . org-upcoming-distant-deadline)))

;;; Blocks

;; The agenda is divided into the following blocks:
;;
;; - agenda: The scheduled tasks and upcoming deadlines
;; - inbox: Tasks that should be refiled to the appropriate location
;; - wait: Blocked tasks
;; - tasks: Tasks that has not been scheduled yet
;;
;; Only "root tasks" are added to the agenda, not the subtasks.
;; A root task is a task who does not have a parent heading.

(defun c/org-agenda--root-task-p ()
  "A root task is a heading at level 1."
  (= 1 (car (org-heading-components))))

(defun c/org-agenda--skip-non-root-tasks ()
  "Skip any task that is not at level 1."
  (save-restriction
    (widen)
    (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
           (next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (cond
       ((c/org-agenda--root-task-p) nil)
       (t next-headline)))))

(setq org-agenda-custom-commands
      `(("c" "Agenda"
         ((agenda "" nil)
          (tags-todo "refile"
                     ((org-agenda-overriding-header "\ninbox --------------------------------------------------------------------------")))
          (todo "WAIT"
                ((org-agenda-overriding-header "\nwait ---------------------------------------------------------------------------")))

          (tags-todo "-HOLD-WAIT-refile/!"
                ((org-agenda-overriding-header "\ntasks --------------------------------------------------------------------------")

                 ;; Never show scheduled tasks - they will appear on the
                 ;; appropriate date
                 (org-agenda-tags-todo-honor-ignore-options t)
                 (org-agenda-todo-ignore-scheduled 'all)

                 ;; Never show tasks with deadline - they will appear when
                 ;; they're within the near future
                 (org-agenda-todo-ignore-deadlines 'all)

                 ;; (org-agenda-skip-function #'c/org-agenda-skip-inbox)
                 (org-agenda-skip-function #'c/org-agenda--skip-non-root-tasks)))))))

;;; Theming

;; Hide separators between agenda blocks
(setq org-agenda-block-separator nil)

;; Hide refile tags (shown in separate section anyways) and category tags
(setq org-agenda-hide-tags-regexp "refile\\|remente\\|personal\\|findity")

;; Remove indentation from agenda blocks
(setq org-agenda-prefix-format
      '((agenda . "%i%-12:c%?-12t% s")
        (todo . "%i%-12:c")
        (tags . "%i%-12:c")
        (search . "%i%-12:c")))

;; Remove header, e.g. Day-agenda (W35)
(setq org-agenda-overriding-header "")

;; Always display agenda with 80 character width
(setq org-agenda-tags-column -80)

;; Don't dim blocked tasks
(setq org-agenda-dim-blocked-tasks nil)

;; Make time grid as wide as the tag column
(setq org-agenda-time-grid
      '((daily today require-timed)
        (800 1000 1200 1400 1600 1800 2000)
        "......" "--------------------------------------------------------"))

(setq org-agenda-current-time-string
      "--------------------------------------------------------")

;; Use shorter prefixes to scheduled/deadlined tasks
(setq org-agenda-scheduled-leaders `("" "(+%1d)"))
(setq org-agenda-deadline-leaders `("(!!)" "(-%1d)" "(+%1d)"))

;; Format the date separators to match the other block separators better
(setq org-agenda-format-date 'c/org-agenda-format-date-aligned)

(defun c/org-agenda-format-date-aligned (date)
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
           (pad-str (make-string padding ?-))
           (pattern (format "%%s%%%ds" padding)))
      (format "%s %s %s" lhs pad-str rhs))))

(provide 'chip-org-agenda)

;;; chip-org-agenda.el ends here
