;;; chip-org.el -*- lexical-binding: t -*-

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

;;; Code:

(require 'cl-lib)
(require 's)

;;; Packages

(use-package org)

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
  (after-load (evil)
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
      (kbd "q") 'org-fc-review-quit)))

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

;;; Scheduling

;; Disable the calendar - doesn't work well when scheduling while capturing a task
;; (setq org-read-date-popup-calendar nil)

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

(setq org-clock-sound "~/audio/waterdrop.wav")

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
        (org-fold-show-set-visibility t)
        (org-fold-show-entry)
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

(defface org-bullet-headline-1
  '((t (:inherit (org-level-1) :underline t)))
  "Face used for content portion of a headline at level 1."
  :group 'org-bullet)

(defface org-bullet-headline-2
  '((t (:inherit (org-level-2) :underline t)))
  "Face used for content portion of a headline at level 2."
  :group 'org-bullet)

(defface org-bullet-headline-3
  '((t (:inherit (org-level-3) :underline t)))
  "Face used for content portion of a headline at level 3."
  :group 'org-bullet)

(defface org-bullet-headline-4
  '((t (:inherit (org-level-4) :underline t)))
  "Face used for content portion of a headline at level 4."
  :group 'org-bullet)

(defface org-bullet-headline-5
  '((t (:inherit (org-level-5) :underline t)))
  "Face used for content portion of a headline at level 5."
  :group 'org-bullet)

(defface org-bullet-headline-6
  '((t (:inherit (org-level-6) :underline t)))
  "Face used for content portion of a headline at level 6."
  :group 'org-bullet)

(defface org-bullet-headline-7
  '((t (:inherit (org-level-7) :underline t)))
  "Face used for content portion of a headline at level 7."
  :group 'org-bullet)

(defface org-bullet-headline-8
  '((t (:inherit (org-level-8) :underline t)))
  "Face used for content portion of a headline at level 8."
  :group 'org-bullet)

(defcustom org-bullet-char ?\#
  "Replacement for * as header prefixes."
  :type 'characterp
  :group 'org-bullet)

(defcustom org-bullet-headline-face
  'org-headline-content
  "Face used for headline text (minus the bullets and spaces)"
  :type 'symbolp
  :group 'org-bullet)

(defun org-bullet--face-for-headline (level)
  (cl-case level
    (1 'org-bullet-headline-1)
    (2 'org-bullet-headline-2)
    (3 'org-bullet-headline-3)
    (4 'org-bullet-headline-4)
    (5 'org-bullet-headline-5)
    (6 'org-bullet-headline-6)
    (7 'org-bullet-headline-7)
    (t 'org-bullet-headline-8)))

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
                          ;; Replace each bullet character with org-bullet-char
                          (dotimes (i level)
                            (put-text-property (+ (match-beginning 1) i)
                                               (+ (match-beginning 1) i 1)
                                               'display
                                               (propertize (char-to-string org-bullet-char) 'face 'org-bullet)))

                          ;; Set face for headline content
                          (if todo
                              (put-text-property (+ (match-beginning 2) (length todo) 1)
                                                 (match-end 2)
                                                 'face
                                                 (org-bullet--face-for-headline level))
                            (put-text-property (match-beginning 2)
                                               (match-end 2)
                                               'face
                                               (org-bullet--face-for-headline level)))
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

;; Activate appointments so we get notifications for Linux systems
(when (string-equal system-type "gnu/linux")
  (c/org-agenda-to-appt)
  (appt-activate t)

  ;; Refresh reminders every 10 minutes
  (run-at-time t (* 60 10) 'c/org-agenda-to-appt))

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

;;; Links

(defun c/yes-or-no-cached-p (prompt)
  (let ((cache-path (concat chip-config-cache-dir "yes-or-no-responses")))

    ;; Ensure cache dir & file exists
    (make-directory chip-config-cache-dir t)
    (unless (file-exists-p cache-path)
      (write-region "()" nil cache-path))

    ;; Read cached responses
    (let ((cached-responses (with-temp-buffer
                              (insert-file-contents cache-path)
                              (read (current-buffer)))))
      (or (cdr (assoc prompt cached-responses))
          (when (yes-or-no-p prompt)
            (let ((remember? (yes-or-no-p "Remember this answer in the future? ")))
              (when remember?
                (push (cons prompt t) cached-responses)

                ;; Write response into cache file
                (with-temp-buffer
                  (insert (prin1-to-string cached-responses))
                  (write-region nil nil cache-path))))
            t)))))

(setq org-confirm-elisp-link-function 'c/yes-or-no-cached-p)

;;; Archiving tasks

;; I have a recurring task scheduled at the beginning of each month reminding me
;; to archive old tasks. The rationale is to keep the agenda files reasonably
;; small for better performance.

;; I use org-ql to query all tasks that are completed or killed at before the
;; beginning of last month. This ensures that I always have at least a month of
;; completed tasks available in case I want to do a clock table.

;; From the org-ql buffer, I can mark all tasks and bulk-archive them using the
;; same keybindings as in the agenda.

(use-package org-ql)

(defun c/beginning-of-last-month ()
  "Get the beginning of last month as a string of format YYYY-MM-dd."
  (let* ((daynr (string-to-number (format-time-string "%d" (current-time))))
         (a-month-ago (* 60 60 24 (+ daynr 1)))
         (last-month (format-time-string "%Y-%m-01" (time-subtract (current-time) (seconds-to-time a-month-ago)))))
    last-month))

(defun c/org-query-old-tasks ()
  "Query all tasks that are completed or killed at before the
beginning of last month."
  (interactive)
  (org-ql-search
    org-agenda-files
    `(and (done)
          (not (parent))
          (or
           (ts-active :to ,(c/beginning-of-last-month))
           (closed :to ,(c/beginning-of-last-month))))))

;; Make sure to not alter task state when archiving
(setq org-archive-mark-done nil)

;;; LaTeX

;; Use a bigger size for LaTeX previews
(setq org-format-latex-options
      (plist-put org-format-latex-options :scale 1.5))

;;; Source blocks

;; Use current window when editing source blocks
(setq org-src-window-setup 'current-window)

;;; Capture templates

(setq org-capture-templates
      `(("t" "TODO" entry (file "~/org/personal/refile.org")
         "* TODO %?")))

;;; Effort estimation

;; Use predetermined effort estimation values
(customize-set-variable 'org-global-properties
                        '(("Effort_ALL" . "0:05 0:10 0:15 0:30 0:45 1:00 2:00 4:00")))

;;; Misc

;; Fold logbooks etc at startup
(setq org-startup-folded 'all)

;; add timestamp to completed todos
(setq org-log-done 'time)

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
  :disabled t
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
;; (setq org-hide-emphasis-markers t)

;; prevent org source blocks from being indented
(setq org-edit-src-content-indentation 0)
(setq org-src-preserve-indentation nil)
(setq org-src-tab-acts-natively nil)

(setq org-tags-column -80)

;; resize image according to #+ATTR.* if available
(setq org-image-actual-width nil)

;; add automatic newlines when lines get too long
;; using this instead of word-wrap since it doesn't affect tables
(add-hook 'org-mode-hook 'auto-fill-mode)

(defun get-presentation-path ()
  "Prompt for presentation name via minibuffer and return path."
  (let ((name (read-from-minibuffer "Presentation name: "))
        (date (shell-command-to-string "echo -n $(date +%Y%m%d)")))
    (format "~/org/remente/presentations/%s-%s/presentation.org" date name)))

(when c/config-evil?
  (add-hook 'org-capture-mode-hook 'evil-insert-state))

(defun get-journal-path ()
  (let ((date (shell-command-to-string "echo -n $(date +%Y-%m-%d)")))
    (find-file (format "~/org/personal/roam/%s.org" date))))

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
          `(:scope agenda :maxlevel 2 :block today :fileskip0 t :compact t)))
    (org-clock-report)))

(defun org-clock-report-week ()
  "Insert clock report for this week."
  (let ((org-clock-clocktable-default-properties
          `(:scope agenda :maxlevel 2 :block thisweek :fileskip0 t :compact t)))
    (org-clock-report)))

(defun org-clock-report-month ()
  "Insert clock report for this month."
  (let* ((org-clock-clocktable-default-properties
          `(:scope agenda :maxlevel 2 :block lastmonth :fileskip0 t :compact t)))
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

(defun chip/dashboard ()
  (interactive)
  (delete-other-windows)
  (split-window-horizontally)
  (c/org-agenda)
  (chip/window-80)
  (window-preserve-size nil t t)
  (switch-to-buffer-other-window "*scratch*")
  (other-window 1))

;; force child TODOs to be done before parent can be done
(setq org-enforce-todo-dependencies t)

;; force checkboxes to be completed before parent can be done
(setq org-enforce-todo-checkbox-dependencies t)

;; enable use of the RESET_CHECK_BOXES property
(require 'org-checklist)

(setq org-stuck-projects (quote ("" nil nil "")))

;; This prevents too many headlines from being folded together when I'm
;; working with collapsed trees.
(setq org-show-entry-below (quote ((default))))

(provide 'chip-org)
