;;; chip-org.el -*- lexical-binding: t -*-

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

(require 'cl-lib)
(require 's)

;;; Packages

(use-package org-contrib
  :config
  ;; enable blocker properties for unnested dependencies
  (require 'org-depend)
  (setq org-depend-tag-blocked nil)

  ;; enable easy templates
  (require 'org-tempo))

(use-package org-fc
  :straight (org-fc
             :type git :repo "https://git.sr.ht/~l3kn/org-fc"
             :files (:defaults "awk" "demo.org"))
  :custom (org-fc-directories '("~/org/personal/lang"))
  :config
  (require 'org-fc-hydra)
  (evil-define-minor-mode-key '(normal insert emacs) 'org-fc-review-flip-mode
    (kbd "RET") 'org-fc-review-flip
    (kbd "n") 'org-fc-review-flip
    (kbd "s") 'org-fc-review-suspend-card
    (kbd "q") 'org-fc-review-quit)

  (evil-define-minor-mode-key '(normal insert emacs) 'org-fc-review-rate-mode
    (kbd "a") 'org-fc-review-rate-again
    (kbd "h") 'org-fc-review-rate-hard
    (kbd "g") 'org-fc-review-rate-good
    (kbd "e") 'org-fc-review-rate-easy
    (kbd "s") 'org-fc-review-suspend-card
    (kbd "q") 'org-fc-review-quit))

;;; Keybindings

(after-load (org general)
  (general-define-key
   :keymaps 'org-mode-map
   "M-p" 'org-previous-visible-heading
   "M-n" 'org-next-visible-heading
   "M-e" 'org-previous-visible-heading
   "M-N" 'org-move-subtree-down
   "M-E" 'org-move-subtree-up
   "M-i" 'org-metaright
   "M-h" 'org-metaleft
   "M-I" 'org-shiftmetaright
   "M-H" 'org-shiftmetaleft
   "C-<return>" 'c/org-insert-heading-respect-content
   "C-S-<return>" 'c/org-insert-todo-heading-respect-content
   "<RET>" 'org-return-indent
   "<tab>" 'org-cycle))

(defun c/org-insert-heading-respect-content ()
  (interactive)
  (org-insert-heading-respect-content)
  (evil-insert-state))

(defun c/org-insert-todo-heading-respect-content ()
  (interactive)
  (org-insert-todo-heading-respect-content)
  (evil-insert-state))

;;; Theming

;; Enable background colors etc in quote and verse blocks
(setq org-fontify-quote-and-verse-blocks t)

;;; Clocking

(defun c/org-add-clocked-time (mins)
  "Add some amount of minutes as a clock entry to the heading at point.
The start timestamp of the clock entry is created `mins' minutes from the current time."
  (interactive "nMinutes: ")
  (save-window-excursion
    (save-excursion
      (when (eq major-mode 'org-agenda-mode)
        (org-agenda-switch-to))
      (goto-char (org-log-beginning t))
      (let* ((now (org-current-time org-clock-rounding-minutes))
             (start (org-time-subtract now (seconds-to-time (* 60 mins)))))
        (save-excursion
          (insert "CLOCK: ")
          (org-insert-time-stamp start 'with-hm 'inactive)
          (insert "--")
          (org-insert-time-stamp now 'with-hm 'inactive)
          (newline))
        (org-evaluate-time-range)))))

;;; Deadlines

;; Deadlines are discretely displayed in the agenda 7 days before due
;; date, and are extra highlighted 2 days before.
(setq org-deadline-warning-days 7)
(setq org-agenda-deadline-faces
      '((1.0 . org-warning)
        (0.7 . org-upcoming-deadline)
        (0.0 . org-upcoming-distant-deadline)))

;;; Attachments

;; Use inheritance for attachments - this allows us to link to an file attached to a parent node
(setq org-attach-use-inheritance t)

;; Use absolute attachment directory so that refiling doesn't break attachments
(setq org-attach-id-dir (concat c/org-dir "attachments"))

(use-package org-attach-screenshot
  :config
  (setq org-attach-screenshot-auto-refresh #'never))

;;; Jump to headline

(defun c/sorted-completion-table (completions)
  "Create a completion table that is sorted by index in list `completions'."
  (lambda (string pred action)
    (if (eq action 'metadata)
        `(metadata (display-sort-function . ,#'identity))
      (complete-with-action action completions string pred))))

(defun c/org-agenda-headlines-candidates ()
  "Return a list of headline completion candidates for use with selectrum."
  (org-map-entries
   (lambda ()
     (cl-destructuring-bind (_ _ todo priority text tags) (org-heading-components)
       (let* ((path (org-get-outline-path)))
         (list (mapconcat 'identity
                          (cl-remove-if 'null
                                        (list
                                         todo
                                         (and priority (format "[#%c]" priority))
                                         (s-join "/" (append path (list text)))
                                         tags))
                          " ")
               buffer-file-name
               (point)))))
   nil
   'agenda))

(defun c/org-jump-to-headline ()
  "Jump to the location of an org headline."
  (interactive)
  (let* ((completions (c/org-agenda-headlines-candidates))
         (completion-table (c/sorted-completion-table completions)))
    (let ((result (assoc (completing-read "Jump to headline " completion-table) completions)))
      (when result
        (find-file (cadr result))
        (goto-char (caddr result))
        (recenter)))))

(defun c/org-agenda-jump-to-task ()
  "Jump to the location of an org task inside the org agenda."
  (interactive)
  (let (results)
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (when (org-get-at-bol 'todo-state)
          (push (cons (org-get-at-bol 'txt) (point)) results))
        (forward-line 1)))
    (setq results (nreverse results))
    (goto-char
     (cdr
      (assoc
       (completing-read "Test: " (c/sorted-completion-table results))
       results)))))

;;; Bullets

(defface org-bullet
  '((t (:inherit (default))))
  "Face used for org-bullets."
  :group 'org-bullet)

(defface org-headline-content
  '((t (:inherit (default))))
  "Face used for content portion of a headline"
  :group 'org-bullet)

(defcustom org-bullet-char ?\#
  "Replacement for * as header prefixes."
  :type 'characterp
  :group 'org-bullet)

(define-minor-mode org-bullet-mode
  "Bullet for org-mode"
  nil nil nil
  (let* ((keyword `(("^\\(\\*+ \\)\\(.*\\)"
                     (0 (let* ((level (- (match-end 1) (match-beginning 1) 1))
                               (header (save-match-data
                                         (save-excursion
                                           (goto-char (match-beginning 1))
                                           (org-heading-components))))
                               (todo (caddr header)))
                          ;; Change bullet
                          (put-text-property (match-beginning 1)
                                             (- (match-end 1) 1)
                                             'display
                                             (propertize (make-string level org-bullet-char) 'face 'org-bullet))
                          ;; Set face for headline content
                          (if todo
                              (put-text-property (+ (match-beginning 2) (length todo) 1)
                                                 (match-end 2)
                                                 'face
                                                 'org-headline-content)
                            (put-text-property (match-beginning 2)
                                                 (match-end 2)
                                                 'face
                                                 'org-headline-content))
                          nil)))
                    ("^\\*+ .*[^\s]+\\([[:blank:]]+\\):.+:"
                     (0 (let* ((spaces (- (match-end 1) (match-beginning 1))))
                          ;; Fix spaces between header text and tags
                          (put-text-property (match-beginning 1)
                                             (match-end 1)
                                             'face
                                             'org-default)
                          nil)))
                    )))
    (if org-bullet-mode
        (progn
          (font-lock-add-keywords nil keyword)
          (font-lock-fontify-buffer))
      (save-excursion
        (goto-char (point-min))
        (font-lock-remove-keywords nil keyword)
        (font-lock-fontify-buffer)))))

(add-hook 'org-mode-hook 'org-bullet-mode)

;;; Notifications

(defun c/org-agenda-to-appt ()
  (interactive)
  (org-agenda-to-appt t))

;; Set up when emacs starts
(c/org-agenda-to-appt)

;; Activate appointments so we get notifications
(appt-activate t)

;; Refresh reminders every 10 minutes
(run-at-time t (* 60 10) 'c/org-agenda-to-appt)

;; Show reminders at 15, 10 and 5 minutes prior
(setq appt-message-warning-time 15)
(setq appt-display-interval 5)

;; Display notifications using the alert package
(defun c/appt-disp-window-function (min-to-app new-time msg)
  ;; Each argument may also be a list in case of multiple appts occuring at the same time
  (let ((min-to-app (if (listp min-to-app) min-to-app (list min-to-app)))
        (msg (if (listp msg) msg (list msg)))
        (new-time (if (listp new-time) new-time (list new-time)))))

  ;; Process message (remove tags if any exists)
  (let ((output (if (string-match "^\\*+ .*[^\s]+\\([[:blank:]]+\\):.+:" msg)
                    (s-trim (substring msg 0 (match-end 1)))
                  (s-trim msg)))
        (severity (if (= (string-to-number min-to-app) 0) 'high 'normal)))
    (alert (format "%s (in %s minutes)" msg min-to-app) :severity severity)))

(setq appt-disp-window-function #'c/appt-disp-window-function)

;;; Archiving

;; Use `c/org-query-archived' to query and archive tasks

(use-package org-ql)

(defun c/beginning-of-last-month ()
  (let* ((daynr (string-to-number (format-time-string "%d" (current-time))))
         (a-month-ago (* 60 60 24 (+ daynr 1)))
         (last-month (format-time-string "%Y-%m-01" (time-subtract (current-time) (seconds-to-time a-month-ago)))))
    last-month))

(defun c/org-query-archived ()
  (interactive)
  (org-ql-search
    org-agenda-files
    `(and (done)
          (not (parent))
          (ts-active :to ,(c/beginning-of-last-month)))))

;;; Misc

;; set org todo keywords
(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "WAIT(w@/!)" "HOLD(h@/!)" "|" "DONE(d)" "KILL(c@)")))

;; add timestamp to completed todos
(setq org-log-done 'time)

;; make sure to not alter TODO state when archiving
(setq org-archive-mark-done nil)

;; create automatic bookmarks for org captures
(setq org-bookmark-names-plist
      '(:last-capture "org:last-capture"))

(setq org-file-apps
         '(("\\.png\\'" . "feh --scale-down \"%s\"")
           ("\\.jpg\\'" . "feh --scale-down \"%s\"")
           (auto-mode . emacs)
           ("\\.mm\\'" . default)
           ("\\.x?html?\\'" . default)
           ("\\.pdf\\'" . default)))

;; Cleanup intermediate files after org export
(setq org-latex-logfiles-extensions '("tex" "spl"))

;; Log state changes into the LOGBOOK drawer
(setq org-log-into-drawer t)

(use-package ob-restclient
  :ensure t)

(use-package org-gcal
  :ensure t
  :after org
  :config
  (setq org-gcal-client-id private/gcal-client-id
        org-gcal-client-secret private/gcal-client-secret
        org-gcal-file-alist `((,private/gcal-calendar-id . "~/org/personal/gcal.org"))))

(use-package ob-http
  :ensure t)

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

;; stolen from: https://writequit.org/articles/emacs-org-mode-generate-ids.html#automating-id-creation
(require 'org-id)
(setq org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)

(use-package ox-pandoc
  :defer t
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

;; TODO make macro that calls this and adds to org-confirm-babel-evaluate
(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)
   (shell . t)
   (js . t)
   (lilypond . t)
   (ditaa . t)
   (restclient . t)
   (scheme . t)
   (emacs-lisp . t)
   (lisp . t)
   (forth . t)
   (julia . t)
   (http . t)))

;; Enable noweb expansion in all languages
(setq org-babel-default-header-args
      (cons '(:noweb . "yes")
            (assq-delete-all :noweb org-babel-default-header-args)))

;; Default to lexical scope in elisp blocks
(setq org-babel-default-header-args:emacs-lisp
      '((:lexical . "yes")))

(defun my-org-confirm-babel-evaluate (lang body)
  (not (member lang '("python" "bash" "js" "typescript" "lisp" "lilypond" "ditaa" "restclient" "scheme" "elisp" "emacs-lisp" "forth"))))

(setq org-confirm-babel-evaluate 'my-org-confirm-babel-evaluate)

;; Redisplay inlined images after source block execution
(add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images)

;; I often want to execute a bash source block in a separate shell buffer in order
;; to monitor the output and interact with it (e.g. inputting passwords). This
;; piece of code enables this behavior when the :buffer parameter is set in the
;; sourch block header.

(defun ob-shell-buffer-org-babel-execute-src-block (&optional orig-fun arg info params)
  (interactive "P")
  (cond
   ;; If this function is not called as advice, do nothing
   ((not orig-fun)
    (warn "This function should not be called interactively")
    nil)
   ;; If there is no :buffer parameter, call the original function
   ((not (assoc :buffer (nth 2 (or info (org-babel-get-src-block-info)))))
    (funcall orig-fun arg info params))
   ;; If not a bash src block, call original function
   ((not (member (nth 0 (or info (org-babel-get-src-block-info))) '("bash" "sh")))
    (funcall orig-fun arg info params))
   ;; Otherwise, send contents to shell process
   (t
    (let ((contents (cadr (org-babel-get-src-block-info)))
          (path (make-temp-file "org-bash-execute")))
      (with-temp-file path
        (insert (format "#!/bin/bash\nset -e\n%s" contents)))
      (chmod path (string-to-number "777" 8))
      ;; (start-process-shell-command "test" buffer (format "bash %s" path))
      (comint-send-string
       (get-buffer-process (shell))
       (format "bash %s\n" path))
      ))))

(advice-add 'org-babel-execute-src-block :around 'ob-shell-buffer-org-babel-execute-src-block)

(setq org-startup-indented nil)
(setq org-adapt-indentation nil)
(setq org-indent-indentation-per-level 2)

;; Hide emphasis markers for a more readable document
(setq org-hide-emphasis-markers t)

;; prevent org source blocks from being indented
(setq org-edit-src-content-indentation 0)
(setq org-src-preserve-indentation nil)
(setq org-src-tab-acts-natively nil)

(setq org-tags-column -80)

;; resize image according to ATTR_ORG if available
(setq org-image-actual-width nil)

;; add automatic newlines when lines get too long
;; using this instead of word-wrap since it doesn't affect tables
(add-hook 'org-mode-hook 'auto-fill-mode)

(defun get-presentation-path ()
  "Prompt for presentation name via minibuffer and return path."
  (let ((name (read-from-minibuffer "Presentation name: "))
        (date (shell-command-to-string "echo -n $(date +%Y%m%d)")))
    (format "~/org/remente/presentations/%s-%s/presentation.org" date name)))

(add-hook 'org-capture-mode-hook 'evil-insert-state)

(defun get-journal-path ()
  (let ((date (shell-command-to-string "echo -n $(date +%Y-%m-%d)")))
    (find-file (format "~/org/personal/roam/%s.org" date))))

(setq org-capture-templates
      `(("t" "TODO" entry (file "~/org/personal/refile.org")
         "* TODO %?")
        ("f" "Log fitness data" table-line (file+headline "~/org/personal/fitness.org" "Weight tracking")
         "| %t | %? |   |")
        ("w" "Workout")
        ("wa" "Workout A" entry #'get-journal-path
         "
* Workout
%T
| Bulgarian Split Squat    | 3x10 | %?  |
| Bench Press              | 3x10 |   |
| Straight-Legged Deadlift | 3x10 |   |
| Plank                    | 3x10 | - |
" :clock-in t :clock-resume t)
        ("wb" "Workout B" entry #'get-journal-path
         "
* Workout
%T
| Bulgarian Split Squat | 3x10 | %?  |
| Seated Shoulder Press | 3x10 |   |
| Bent Over Row         | 3x10 |   |
| Plank                 | 3x10 | - |
" :clock-in t :clock-resume t)
        ("c" "Daily checklist" entry (file "~/org/remente/remente.org")
         "* TODO Daily checklist
- [ ] Check slack%?
- [ ] Check email
- [ ] Check missive"
         :clock-in t
         :clock-resume t)))

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

(setq org-refile-targets '((nil :maxlevel . 3)
                           (org-agenda-files :maxlevel . 3)))
(setq org-refile-use-outline-path 'file)
(setq org-outline-path-complete-in-steps nil)

(defun chip/verify-refile-target ()
  "Exclude todo keywords with a done state from refile targets"
  (not (member (nth 2 (org-heading-components)) org-done-keywords)))

(setq org-refile-target-verify-function 'chip/verify-refile-target)

;; remove clock entry if total time span is less than one minute
(setq org-clock-out-remove-zero-time-clocks t)

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

(defun chip/org-agenda ()
  (interactive)
  (org-agenda nil "c")
  (org-agenda-redo-all))

(defun chip/dashboard ()
  (interactive)
  (delete-other-windows)
  (split-window-horizontally)
  (chip/org-agenda)
  (chip/window-80)
  (window-preserve-size nil t t)
  (switch-to-buffer-other-window "*scratch*")
  (other-window 1))

(require 'org-habit)

(setq org-agenda-files
      `(,(concat c/org-dir "personal/personal.org")
        ,(concat c/org-dir "personal/refile.org")
        ,(concat c/org-dir "remente/remente.org")))

;; keep agenda filters after closing agenda buffer
(setq org-agenda-persistent-filter t)

;; prevent org-agenda from destroying splits
(setq org-agenda-window-setup 'current-window)

;; always start agenda on current day instead of mondays
(setq org-agenda-start-on-weekday nil)

;; show only today as default
(setq org-agenda-span 'day)

;; bury agenda buffer instead of killing it on quit
(setq org-agenda-sticky t)

(setq org-agenda-sorting-strategy
      '((agenda habit-down time-up todo-state-up priority-down category-keep)
       (todo priority-down category-keep)
       (tags priority-down category-keep)
       (search category-keep)))

;; force child TODOs to be done before parent can be done
(setq org-enforce-todo-dependencies t)

;; force checkboxes to be completed before parent can be done
(setq org-enforce-todo-checkbox-dependencies t)

;; enable use of the RESET_CHECK_BOXES property
(require 'org-checklist)

(setq org-stuck-projects (quote ("" nil nil "")))

(setq org-agenda-tags-column -80)

;; hide separators between agenda blocks
(setq org-agenda-block-separator nil)

;; don't dim blocked tasks
(setq org-agenda-dim-blocked-tasks nil)

;; remove agenda indentation
(setq org-agenda-prefix-format
      '((agenda . "%i%-12:c%?-12t% s")
        (todo . "%i%-12:c")
        (tags . "%i%-12:c")
        (search . "%i%-12:c")))

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
           (pad-str (make-string padding ?-))
           (pattern (format "%%s%%%ds" padding)))
      (format "%s %s %s" lhs pad-str rhs))))

;; set agenda icons
(setq org-agenda-scheduled-leaders `("" "(+%1d)"))
(setq org-agenda-deadline-leaders `("(!!)" "(-%1d)" "(+%1d)"))

;; make time grid as wide as the tag column
(setq org-agenda-time-grid
      '((daily today require-timed)
        (800 1000 1200 1400 1600 1800 2000)
        "......" "--------------------------------------------------------"))
(setq org-agenda-current-time-string
      "--------------------------------------------------------")

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

(defun bh/is-subproject-p ()
  "Any task which is a subtask of another project"
  (let ((is-subproject)
        (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
    (save-excursion
      (while (and (not is-subproject) (org-up-heading-safe))
        (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
          (setq is-subproject t))))
    (and is-a-task is-subproject)))

(defun bh/list-sublevels-for-projects-indented ()
  "Set org-tags-match-list-sublevels so when restricted to a subtree we list all subtasks.
  This is normally used by skipping functions where this variable is already local to the agenda."
  (if (marker-buffer org-agenda-restrict-begin)
      (setq org-tags-match-list-sublevels 'indented)
    (setq org-tags-match-list-sublevels nil))
  nil)

(defun bh/list-sublevels-for-projects ()
  "Set org-tags-match-list-sublevels so when restricted to a subtree we list all subtasks.
  This is normally used by skipping functions where this variable is already local to the agenda."
  (if (marker-buffer org-agenda-restrict-begin)
      (setq org-tags-match-list-sublevels t)
    (setq org-tags-match-list-sublevels nil))
  nil)

(defvar bh/hide-scheduled-and-waiting-next-tasks t)

(defun bh/toggle-next-task-display ()
  (interactive)
  (setq bh/hide-scheduled-and-waiting-next-tasks (not bh/hide-scheduled-and-waiting-next-tasks))
  (when  (equal major-mode 'org-agenda-mode)
    (org-agenda-redo))
  (message "%s WAIT and SCHEDULED NEXT Tasks" (if bh/hide-scheduled-and-waiting-next-tasks "Hide" "Show")))

(defun bh/skip-stuck-projects ()
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
                nil
              next-headline)) ; a stuck project, has subtasks but no next task
        nil))))

(defun bh/skip-non-projects ()
  "Skip trees that are not projects"
  (save-restriction
    (widen)
    (let ((subtree-end (save-excursion (org-end-of-subtree t))))
      (cond
       ((bh/is-project-p)
        nil)
       ((and (bh/is-project-subtree-p) (not (bh/is-task-p)))
        nil)
       (t
        subtree-end)))))

(defun bh/skip-non-tasks ()
  "Show non-project tasks.
Skip project and sub-project tasks, habits, and project related tasks."
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (cond
       ((bh/is-task-p)
        nil)
       (t
        next-headline)))))

(defun bh/skip-project-tasks ()
  "Show non-project tasks.
Skip project and sub-project tasks, habits, and project related tasks."
  (save-restriction
    (widen)
    (let* ((subtree-end (save-excursion (org-end-of-subtree t))))
      (cond
       ((bh/is-project-p)
        subtree-end)
       ((org-is-habit-p)
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
       ((org-is-habit-p)
        subtree-end)
       ((and (bh/is-project-subtree-p)
             (member (org-get-todo-state) (list "NEXT")))
        subtree-end)
       ((not (bh/is-project-subtree-p))
        subtree-end)
       (t
        nil)))))

(setq org-agenda-custom-commands
      (quote (("N" "Notes" tags "NOTE"
               ((org-agenda-overriding-header "Notes")
                (org-tags-match-list-sublevels t)))
              ("h" "Habits" tags-todo "STYLE=\"habit\""
               ((org-agenda-overriding-header "Habits")
                (org-agenda-sorting-strategy
                 '(todo-state-down effort-up category-keep))))
              ("c" "Agenda"
               ((agenda "" nil)
                (tags "refile"
                      ((org-agenda-overriding-header "\nrefile -------------------------------------------------------------------------")
                       (org-tags-match-list-sublevels nil)))
                (tags-todo "-refile-KILL/!"
                           ((org-agenda-overriding-header
                             (if bh/hide-scheduled-and-waiting-next-tasks
                                 "\ntasks --------------------------------------------------------------------------"
                               "\ntasks (+wait +sched) -----------------------------------------------------------"))
                            (org-agenda-skip-function 'bh/skip-project-tasks)
                            (org-agenda-tags-todo-honor-ignore-options t)
                            (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-todo-ignore-with-date bh/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-sorting-strategy
                             '(category-keep))))
                (tags-todo
                 "-hold-KILL/!"
                           ((org-agenda-overriding-header "\nprojects -----------------------------------------------------------------------")
                            (org-agenda-skip-function 'bh/skip-non-projects)
                            (org-tags-match-list-sublevels 'indented)
                            (org-agenda-sorting-strategy
                             '(category-keep))))
                (tags-todo "-hold-refile-KILL/!"
                           ((org-agenda-overriding-header
                             (if bh/hide-scheduled-and-waiting-next-tasks
                                 "subtasks -----------------------------------------------------------------------"
                               "subtasks (+wait +sched) --------------------------------------------------------"))
                            (org-agenda-skip-function 'bh/skip-non-project-tasks)
                            (org-agenda-tags-todo-honor-ignore-options t)
                            (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-todo-ignore-with-date bh/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-sorting-strategy
                             '(category-keep)))))
               nil))))

(general-define-key
 "C-c o n" 'bh/org-todo
 "C-c o w" 'bh/widen)

(setq org-todo-state-tags-triggers
      (quote (("KILL" ("KILL" . t))
              ("WAIT" ("WAIT" . t))
              ("HOLD" ("WAIT") ("hold" . t))
              (done ("WAIT") ("hold"))
              ("TODO" ("WAIT") ("KILL") ("hold"))
              ("NEXT" ("WAIT") ("KILL") ("hold"))
              ("DONE" ("WAIT") ("KILL") ("hold")))))

(defun chip/org-auto-exclude-function (tag)
  "Automatic task exclusion in the agenda with / RET"
  (and (cond
        ((string= tag "hold")
         t))
       (concat "-" tag)))

(setq org-agenda-auto-exclude-function 'chip/org-auto-exclude-function)

;; - T (tasks) for C-c / t on the current buffer
;; - N (narrow) narrows to this task subtree
;; - U (up) narrows to the immediate parent task subtree without moving
;; - P (project) narrows to the parent project subtree without moving
;; - F (file) narrows to the current file or file of the existing restriction

(defun bh/org-todo (arg)
  (interactive "p")
  (if (equal arg 4)
      (save-restriction
        (bh/narrow-to-org-subtree)
        (org-show-todo-tree nil))
    (bh/narrow-to-org-subtree)
    (org-show-todo-tree nil)))

(defun bh/widen ()
  (interactive)
  (if (equal major-mode 'org-agenda-mode)
      (progn
        (org-agenda-remove-restriction-lock)
        (when org-agenda-sticky
          (org-agenda-redo)))
    (widen)))

(add-hook 'org-agenda-mode-hook
          #'(lambda () (org-defkey org-agenda-mode-map "W" (lambda () (interactive) (setq bh/hide-scheduled-and-waiting-next-tasks t) (bh/widen))))
          'append)

(defun bh/restrict-to-file-or-follow (arg)
  "Set agenda restriction to 'file or with argument invoke follow mode.
I don't use follow mode very often but I restrict to file all the time
so change the default 'F' binding in the agenda to allow both"
  (interactive "p")
  (if (equal arg 4)
      (org-agenda-follow-mode)
    (widen)
    (bh/set-agenda-restriction-lock 4)
    (org-agenda-redo)
    (beginning-of-buffer)))

(add-hook 'org-agenda-mode-hook
          #'(lambda () (org-defkey org-agenda-mode-map "F" 'bh/restrict-to-file-or-follow))
          'append)

(defun bh/narrow-to-org-subtree ()
  ;; (widen)
  ;; (org-narrow-to-subtree)
  (save-restriction
    (org-agenda-set-restriction-lock)))

(defun bh/narrow-to-subtree ()
  (interactive)
  (if (equal major-mode 'org-agenda-mode)
      (progn
        (org-with-point-at (org-get-at-bol 'org-hd-marker)
          (bh/narrow-to-org-subtree))
        (when org-agenda-sticky
          (org-agenda-redo)))
    (bh/narrow-to-org-subtree)))

(add-hook 'org-agenda-mode-hook
          #'(lambda () (org-defkey org-agenda-mode-map "N" 'bh/narrow-to-subtree))
          'append)

(defun bh/narrow-up-one-org-level ()
  (widen)
  (save-excursion
    (outline-up-heading 1 'invisible-ok)
    (bh/narrow-to-org-subtree)))

(defun bh/get-pom-from-agenda-restriction-or-point ()
  (or (and (marker-position org-agenda-restrict-begin) org-agenda-restrict-begin)
      (org-get-at-bol 'org-hd-marker)
      (and (equal major-mode 'org-mode) (point))
      org-clock-marker))

(defun bh/narrow-up-one-level ()
  (interactive)
  (if (equal major-mode 'org-agenda-mode)
      (progn
        (org-with-point-at (bh/get-pom-from-agenda-restriction-or-point)
          (bh/narrow-up-one-org-level))
        (org-agenda-redo))
    (bh/narrow-up-one-org-level)))

(add-hook 'org-agenda-mode-hook
          #'(lambda () (org-defkey org-agenda-mode-map "U" 'bh/narrow-up-one-level))
          'append)

(defun bh/narrow-to-org-project ()
  (widen)
  (save-excursion
    (bh/find-project-task)
    (bh/narrow-to-org-subtree)))

(defun bh/narrow-to-project ()
  (interactive)
  (if (equal major-mode 'org-agenda-mode)
      (progn
        (org-with-point-at (bh/get-pom-from-agenda-restriction-or-point)
          (bh/narrow-to-org-project)
          (save-excursion
            (bh/find-project-task)
            (org-agenda-set-restriction-lock)))
        (org-agenda-redo)
        (beginning-of-buffer))
    (bh/narrow-to-org-project)
    (save-restriction
      (org-agenda-set-restriction-lock))))

(add-hook 'org-agenda-mode-hook
          #'(lambda () (org-defkey org-agenda-mode-map "P" 'bh/narrow-to-project))
          'append)

(defvar bh/project-list nil)

(defun bh/view-next-project ()
  (interactive)
  (let (num-project-left current-project)
    (unless (marker-position org-agenda-restrict-begin)
      (goto-char (point-min))
      ; Clear all of the existing markers on the list
      (while bh/project-list
        (set-marker (pop bh/project-list) nil))
      (re-search-forward "Tasks to Refile")
      (forward-visible-line 1))

    ; Build a new project marker list
    (unless bh/project-list
      (while (< (point) (point-max))
        (while (and (< (point) (point-max))
                    (or (not (org-get-at-bol 'org-hd-marker))
                        (org-with-point-at (org-get-at-bol 'org-hd-marker)
                          (or (not (bh/is-project-p))
                              (bh/is-project-subtree-p)))))
          (forward-visible-line 1))
        (when (< (point) (point-max))
          (add-to-list 'bh/project-list (copy-marker (org-get-at-bol 'org-hd-marker)) 'append))
        (forward-visible-line 1)))

    ; Pop off the first marker on the list and display
    (setq current-project (pop bh/project-list))
    (when current-project
      (org-with-point-at current-project
        (setq bh/hide-scheduled-and-waiting-next-tasks nil)
        (bh/narrow-to-project))
      ; Remove the marker
      (setq current-project nil)
      (org-agenda-redo)
      (beginning-of-buffer)
      (setq num-projects-left (length bh/project-list))
      (if (> num-projects-left 0)
          (message "%s projects left to view" num-projects-left)
        (beginning-of-buffer)
        (setq bh/hide-scheduled-and-waiting-next-tasks t)
        (error "All projects viewed.")))))

(add-hook 'org-agenda-mode-hook
          #'(lambda () (org-defkey org-agenda-mode-map "V" 'bh/view-next-project))
          'append)

;; This prevents too many headlines from being folded together when I'm
;; working with collapsed trees.
(setq org-show-entry-below (quote ((default))))

;; =C-c C-x <= turns on the agenda restriction lock for the current
;; subtree.  This keeps your agenda focused on only this subtree.  Alarms
;; and notifications are still active outside the agenda restriction.
;; =C-c C-x >= turns off the agenda restriction lock returning your
;; agenda view back to normal.

;; I have added key bindings for the agenda to allow using =C-c C-x <= in
;; the agenda to set the restriction lock to the current task directly.
;; The following elisp accomplishes this.

(add-hook 'org-agenda-mode-hook
          #'(lambda () (org-defkey org-agenda-mode-map "\C-c\C-x<" 'bh/set-agenda-restriction-lock))
          'append)

(defun bh/set-agenda-restriction-lock (arg)
  "Set restriction lock to current task subtree or file if prefix is specified"
  (interactive "p")
  (let* ((pom (bh/get-pom-from-agenda-restriction-or-point))
         (tags (org-with-point-at pom (org-get-tags-at))))
    (let ((restriction-type (if (equal arg 4) 'file 'subtree)))
      (save-restriction
        (cond
         ((and (equal major-mode 'org-agenda-mode) pom)
          (org-with-point-at pom
            (org-agenda-set-restriction-lock restriction-type))
          (org-agenda-redo))
         ((and (equal major-mode 'org-mode) (org-before-first-heading-p))
          (org-agenda-set-restriction-lock 'file))
         (pom
          (org-with-point-at pom
            (org-agenda-set-restriction-lock restriction-type))))))))

;; Limit restriction lock highlighting to the headline only
(setq org-agenda-restriction-lock-highlight-subtree nil)

(provide 'chip-org)
