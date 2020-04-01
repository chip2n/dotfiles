(provide 'init-org)

;; set org todo keywords
(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "WAIT(w@/!)" "HOLD(h@/!)" "|" "DONE(d)" "KILL(c@)")))

;; add timestamp to completed todos
(setq org-log-done 'time)

;; prevent indentation after sections
(setq org-adapt-indentation nil)

;; prevent org source blocks from being indented
(setq org-edit-src-content-indentation 0)

(setq org-tags-column -80)

;; resize image according to ATTR_ORG if available
(setq org-image-actual-width nil)

;; make sure to not alter TODO state when archiving
(setq org-archive-mark-done nil)

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

(setq org-capture-templates
      `(("t" "TODO" entry (file "~/org/personal/refile.org")
         "* TODO %?")
        ("j" "Journal")
        ("je" "Entry" entry #'org-roam-today
         "* %?\n%T")
        ("js" "Day summary" entry #'org-roam-today
         "* Day summary\n%T\n%?\n\n%(org-clock-report-today)")
        ("ju" "Supplements" entry #'org-roam-today
         "* Supplements\n%T\n| %? |  |")
        ("w" "Workout")
        ("wa" "Workout A" entry #'org-roam-today
         "
* Workout
%T
| Bulgarian Split Squat    | 3x10 | %?  |
| Bench Press              | 3x10 |   |
| Straight-Legged Deadlift | 3x10 |   |
| Plank                    | 3x10 | - |
" :clock-in t :clock-resume t)
        ("wb" "Workout B" entry #'chip/roam-journal-path-today
         "
* Workout
%T
| Bulgarian Split Squat | 3x10 | %?  |
| Seated Shoulder Press | 3x10 |   |
| Bent Over Row         | 3x10 |   |
| Plank                 | 3x10 | - |
" :clock-in t :clock-resume t)
        ("i" "Idea" entry (file+olp "~/org/personal/ideas.org" "Ideas")
         "* %?" :prepend t)
        ("p" "Remente presentation" entry #',(lambda () (find-file (get-presentation-path)))
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

;; enable blocker properties for unnested dependencies
(require 'org-depend)
(setq org-depend-tag-blocked nil)
