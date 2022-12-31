;; -*-lisp-*-
;;
;; My stumpwm configuration

;; TODO: Refresh bar every 10s
;; TODO: Refresh bar on mullvad status change
;; TODO: Make "widgets" abstraction for bar - let them refresh at their own pace (cache the output of the other scripts)
;; TODO: Add possibility of naming windows manually (e.g. "firefox" -> "video"). Or should I use tags?
;; TODO: Format VPN
;; TODO: Move lemonbar to separate package (and publish module)
;; TODO: Prevent multiple bar refreshes that happens close together (e.g. when switching focus rapidly)

(in-package :stumpwm)

(ql:quickload :slynk)
(ql:quickload :alexandria)
(ql:quickload :serapeum)
(ql:quickload :str)
(ql:quickload :cl-ppcre)
(ql:quickload :bordeaux-threads)
(ql:quickload :parse-float)

(defvar config-dir
  (directory-namestring
   (truename (merge-pathnames (user-homedir-pathname) ".stumpwm.d")))
  "Stumpwm config directory.")

(defun load-custom (filename)
  (let ((file (merge-pathnames (concat filename ".lisp") config-dir)))
    (if (probe-file file)
        (load file)
        (format *error-output* "Module '~a' not found." file))))

(defun load-custom-asdf (filename)
  (let ((file (merge-pathnames (concat filename ".asd") config-dir)))
    (if (probe-file file)
        (asdf:operate 'asdf:load-op file)
        (format *error-output* "Module '~a' not found." file))))

(defparameter *window-names*
  '(("Emacs" . "emacs")
    ("st-256color" . "term")
    ("spotify" . "spotify")
    ("firefox" . "firefox")
    ("Discord" . "discord")
    ("Slack" . "slack")
    ("jetbrains-studio" . "android-studio")
    ("google-chrome" . "google-chrome")
    ("signal" . "signal")))

(ql:quickload :rofi)
(setf rofi:*window-names* *window-names*)

;;; * Window tags

(load-module "windowtags")
(add-hook *new-window-hook*
          (lambda (win)
            (if (string-equal (window-class win) "Emacs")
                (windowtags:tag-window "emacs" win))))

;;; * Status bar

;; TODO If no bar is reading fifo, this blocks indefinitely and makes the WM unresponsive!
;;      Can we write in a thread to avoid this?
(defun lemonbar-write (s)
  (let ((path #P"/tmp/panel.fifo"))
    (when (probe-file path)
      (with-open-file (fifo path :direction :output :if-exists :append)
        (format fifo "~a~%" s)))))

(defun lemonbar-mullvad-status ()
  (let ((output (run-shell "mullvad status")))
    (if (ppcre:scan "Tunnel status: Connected" (str:trim output))
        "%{F#c5fca4}✓ VPN%{F-}"
        "%{F#ffa398}❌ VPN%{F-}")))

(defun run-shell (cmd)
  (str:trim
   (with-output-to-string (s)
     (uiop:run-program cmd :output s))))

;;;; * CPU usage

(defvar *cpu-usage* 0)

(defun lemonbar-cpu ()
  (format nil " ~,2f%" *cpu-usage*))

;; Sample CPU usage every 30 seconds
(defvar *lemonbar-cpu-usage-thread* nil)
(defun lemonbar-start-cpu-usage-thread ()
  (setf *lemonbar-cpu-usage-thread*
        (bordeaux-threads:make-thread
         (lambda ()
           (sleep 30)
           (setf *cpu-usage*
                 (parse-float:parse-float
                  (run-shell "mpstat 1 1 | awk '$13 ~ /[0-9.]+/ { print 100 - $13 }'")))
           (lemonbar-start-cpu-usage-thread)))))

;; (defun lemonbar-get-cpu-usage ()
;;   (bordeaux-threads:make-thread
;;    (lambda ()
;;      (setf *cpu-usage* (run-shell "mpstat 1 1 | awk '$13 ~ /[0-9.]+/ { print 100 - $13\"%\" }'"))
;;      (lemonbar-refresh))))

(defun lemonbar-refresh ()
  (lemonbar-write (format nil " λ ~a%{c}~a%{r}~a | ~a | ~a | ~a "
                          (lemonbar-current-group)
                          (lemonbar-windows)
                          (lemonbar-mullvad-status)
                          (lemonbar-cpu)
                          (lemonbar-date)
                          (lemonbar-clock))))

(defun lemonbar-current-group ()
  (group-name (current-group)))

(defun lemonbar-clock ()
  (str:concat " " (run-shell "date +'%H:%M'")))

(defun lemonbar-date ()
  (str:concat " " (run-shell "date +'%Y-%m-%d'")))

(defparameter *lemonbar-window-names* nil
  "Text that should be displayed instead of the window class.")

(setf *lemonbar-window-names* *window-names*)

(defun lemonbar-windows ()
  (let* ((windows (sort-windows-by-number
                   (group-windows (current-group))))
         (names nil))
    (loop for win in windows
          do (push (if (eq (current-window) win)
                       (format nil "%{F#FFFFFF}~a%{F#6b7385}" (lemonbar-format-window-name win))
                       (format nil "%{F#6b7385}~a%{F#6b7385}" (lemonbar-format-window-name win)))
                   names))
    (str:concat
     (str:join " | " (nreverse names))
     "%{F#FFFFFF}")))

(defun lemonbar-format-window-name (win)
  (let* ((class (window-class win))
         (name (cdr (assoc class *lemonbar-window-names* :test #'string-equal))))
    (or name (window-name win))))

(defun lemonbar-init ()
  (add-hook *focus-window-hook*
            (lambda (win last)
              (declare (ignore win last))
              (lemonbar-refresh)))
  ;; Refresh every 10 seconds - this can be way smarter!
  (lemonbar-start-refresh-every 10)
  (lemonbar-start-cpu-usage-thread))

(defvar *lemonbar-refresh-thread* nil)
(defun lemonbar-start-refresh-every (seconds)
  (setf *lemonbar-refresh-thread*
        (bordeaux-threads:make-thread
         (lambda ()
           (sleep seconds)
           (lemonbar-refresh)
           (lemonbar-start-refresh-every seconds)))))

(run-shell-command "lemonbar_start")
(lemonbar-init)


;;; * Misc

(defparameter *color-bg* "#21242b")
(defparameter *color-bg-light* "#282c34")
(defparameter *bar-height* 18)

(setf *startup-message* nil)
(setf *shell-program* (getenv "SHELL"))

(set-prefix-key (kbd "s-t"))

;; Launch dmenu
(define-key *top-map* (kbd "s-p") (format nil "exec dmenu_run -h ~a -nb '~a'" *bar-height* *color-bg-light*))

;; Launch rofi (window switching)
(define-key *top-map* (kbd "s-b") "rofi-windows")
(define-key *top-map* (kbd "s-B") "rofi-pull-global")

;; Launch terminal
(define-key *top-map* (kbd "s-RET") "exec st")

;; Closing windows
(define-key *top-map* (kbd "s-C") "delete")

;; Focus movement
(define-key *top-map* (kbd "s-i") "move-focus right")
(define-key *top-map* (kbd "s-h") "move-focus left")
(define-key *top-map* (kbd "s-e") "move-focus up")
(define-key *top-map* (kbd "s-n") "move-focus down")
(define-key *top-map* (kbd "s-TAB") "pull-hidden-next")
(define-key *top-map* (kbd "s-ISO_Left_Tab") "pull-hidden-previous")

;; Window movement
(define-key *top-map* (kbd "s-H") "move-window left")
(define-key *top-map* (kbd "s-N") "move-window down")
(define-key *top-map* (kbd "s-E") "move-window up")
(define-key *top-map* (kbd "s-I") "move-window right")

;; Splitting
(defcommand split-right () ()
  (hsplit)
  (move-focus :right))

(defcommand split-left () ()
  (hsplit))

(defcommand split-up () ()
  (vsplit))

(defcommand split-down () ()
  (vsplit)
  (move-focus :down))

(define-key *top-map* (kbd "s-C-i") "split-right")
(define-key *top-map* (kbd "s-C-h") "split-left")
(define-key *top-map* (kbd "s-C-e") "split-up")
(define-key *top-map* (kbd "s-C-n") "split-down")

;; Group management
(define-key *top-map* (kbd "s-g") "gselect")

;; Setup groups
(setf (group-name (car (screen-groups (current-screen)))) "main")
(gnewbg "web")

;; TODO Create main layout with predictable frame number
(define-frame-preference "main"
  (4 nil t :class "Game"))

;; TODO For fitting polybar at the top?
;; https://www.reddit.com/r/unixporn/comments/6vdgu2/question_how_to_make_polybar_be_on_top_of_the/

;; Frame resizing
;; (define-key *resize-map* (kbd "h") "resize-direction left")
;; (define-key *resize-map* (kbd "n") "resize-direction down")
;; (define-key *resize-map* (kbd "e") "resize-direction up")
;; (define-key *resize-map* (kbd "i") "resize-direction right")
;; (define-interactive-keymap (c/iresize tile-group) (:on-enter #'setup-iresize
;;                                                    :on-exit #'resize-unhide
;;                                                    :abort-if #'abort-resize-p)

;;   ((kbd "Up") "resize-direction up")
;;   ((kbd "C-p") "resize-direction up")
;;   ((kbd "p") "resize-direction up")
;;   ((kbd "k") "resize-direction up")

;;   ((kbd "Down") "resize-direction down")
;;   ((kbd "C-n") "resize-direction down")
;;   ((kbd "n") "resize-direction down")
;;   ((kbd "j") "resize-direction down")

;;   ((kbd "Left") "resize-direction left")
;;   ((kbd "C-b") "resize-direction left")
;;   ((kbd "b") "resize-direction left")
;;   ((kbd "h") "resize-direction left")

;;   ((kbd "Right") "resize-direction right")
;;   ((kbd "C-f") "resize-direction right")
;;   ((kbd "f") "resize-direction right")
;;   ((kbd "l") "resize-direction right"))

;; Mode line
(setf *screen-mode-line-format*
      '("[^B%n^b] %W"
        "^>"
        "Hello"))

(defcommand slynk (port) ((:string "Port number: "))
  (sb-thread:make-thread
   (lambda ()
     (slynk:create-server :port (parse-integer port) :dont-close t))
   :name "slynk-manual"))

(slynk "4004")

(set-font "-xos4-terminus-medium-r-normal--14-140-72-72-c-80-iso10646-*")

;; (set-font (make-instance 'xft:font
;;                          :family "Iosevka"
;;                          :subfamily "Regular"
;;                          :size 10))

;;; * Frames

;; (defcommand c/setup-group-main () ()
;;   (only)
;;   (run-shell-command "firefox")
;;   (hsplit)
;;   (move-focus :right)
;;   (run-shell-command "st")
;;   (vsplit))

;; (defvar c/main-frames nil)

;; (defun c/make-main-frames ()
;;   (let* ((screen (current-screen))
;;          (head (current-head))
;;          (s-width (head-width head))
;;          (s-height (head-height head))
;;          (f0-width (/ s-width 2))
;;          (f0 (make-frame
;;               :number 0
;;               :x 0 :y 0
;;               :width f0-width
;;               :height s-height))
;;          ;; (f1 (make-frame
;;          ;;      :number 1
;;          ;;      :x 0 :y f0-height
;;          ;;      :width f0-width
;;          ;;      :height (- s-height f0-height)))
;;          ;; (f2 (make-frame
;;          ;;      :number 2
;;          ;;      :x f0-width :y 0
;;          ;;      :width (- s-width f0-width)
;;          ;;      :height s-height))
;;          )
;;     (list f0)
;;     ;; (list f0 f2 f1)
;;     ))

;; (defun c/set-frames (frames &optional (populatep t))
;;   "Display FRAMES in the current group.
;; The first frame will become the current one and will contain the current
;; window.  If POPULATEP is nil, do not populate the rest frames with
;; windows."
;;   (let* ((screen (current-screen))
;;          (group (screen-current-group screen))
;;          (head (current-head group))
;;          (cur-window (group-current-window group))
;;          (cur-frame (first frames)))
;;     (mapc (lambda (w)
;;             (setf (window-frame w) cur-frame))
;;           (group-windows group))
;;     (mapc (lambda (f)
;;             (setf (frame-window f) nil))
;;           (rest frames))
;;     (setf (frame-window cur-frame) cur-window
;;           (tile-group-frame-head group head) frames)
;;     (when populatep
;;       (populate-frames group))
;;     (focus-frame group cur-frame)
;;     (update-decoration cur-window)
;;     (sync-frame-windows group cur-frame)))

;; (defcommand c/main-frames (&optional (populatep t)) ()
;;   (c/set-frames (or c/main-frames
;;                     (setf c/main-frames (c/make-main-frames)))
;;                 populatep))

;;; * Colors

(set-bg-color *color-bg*)
(set-focus-color "#ffffff")
(setf *default-bg-color* *color-bg*)
(set-win-bg-color *color-bg*)
(setf *window-border-style* :thin)
(setf *normal-border-width* 1)
(set-border-color "#ffffff")
(set-unfocus-color *color-bg-light*)

(setf *mode-line-border-width* 0)
(setf *mode-line-pad-y* 2)
(setf *mode-line-background-color* *color-bg-light*)
(setf *mode-line-foreground-color* "#ffffff")

;; Focus windows by clicking on them
(setf *mouse-focus-policy* :click)

;; Enable stumpwm modeline
;; (toggle-mode-line (stumpwm:current-screen) (stumpwm:current-head))

;; Ignore incremental WM size hints (this removes padding normalle added to emacs, terminals etc)
(setf *ignore-wm-inc-hints* t)
