;;; -*- lexical-binding: t; -*-
;; doom-mode-line > mood-line > mode-line+
;; * Deps

(declare-function flycheck-count-errors "ext:flycheck" (errors))

;; ** All the icons
(require 'all-the-icons)

;; * Config

(defgroup mode-line+ nil
  "A minimal mode-line configuration inspired by doom-modeline."
  :group 'mode-line)

(defface mode-line-status-info+
  '((t (:inherit (font-lock-keyword-face))))
  "Face used for generic status indicators in the mode-line."
  :group 'mode-line+)

(defface mode-line-status-good+
  '((t (:inherit (success))))
  "Face used for success status indicators in the mode-line."
  :group 'mode-line+)

(defface mode-line-status-warning+
  '((t (:inherit (warning))))
  "Face for warning status indicators in the mode-line."
  :group 'mode-line+)

(defface mode-line-status-error+
  '((t (:inherit (error))))
  "Face for error stauts indicators in the mode-line."
  :group 'mode-line+)

(defface mode-line-status-grayed-out+
  '((t (:inherit (font-lock-doc-face))))
  "Face used for neutral or inactive status indicators in the mode-line."
  :group 'mode-line+)

(defface mode-line-unimportant+
  '((t (:inherit (font-lock-doc-face))))
  "Face used for less important mode-line elements."
  :group 'mode-line+)

(defface mode-line-modified+
  '((t (:inherit (error))))
  "Face used for the 'modified' indicator symbol in the mode-line."
  :group 'mode-line+)

(defface mode-line-evil-state-normal
  '((t (:inherit (font-lock-keyword-face))))
  "Face used for generic status indicators in the mode-line."
  :group 'mode-line+)

(defface mode-line-evil-state-insert
  '((t (:inherit (font-lock-keyword-face))))
  "Face used for generic status indicators in the mode-line."
  :group 'mode-line+)

(defface mode-line-evil-state-visual
  '((t (:inherit (font-lock-keyword-face))))
  "Face used for generic status indicators in the mode-line."
  :group 'mode-line+)

(defface mode-line-evil-state-motion
  '((t (:inherit (font-lock-keyword-face))))
  "Face used for generic status indicators in the mode-line."
  :group 'mode-line+)

(defface mode-line-evil-state-emacs
  '((t (:inherit (font-lock-keyword-face))))
  "Face used for generic status indicators in the mode-line."
  :group 'mode-line+)

;; * Helper functions

(defvar mode-line--current-window+ (frame-selected-window))

(defun mode-line--update-selected-window+ (&rest _)
  "Keep track of active window.

Store the active window in `mode-line--current-window+' variable."
  (let ((window (frame-selected-window)))
    (when (and (windowp window)
               (not (minibuffer-window-active-p window)))
      (setq mode-line--current-window+ window))))

;; Define a helper function to determine whether or not the current window is active.
(defun mode-line-face-if-active+ (active &optional inactive)
  "Return FACE for current window."
  (if (mode-line-is-active+)
      active
    inactive))

(defsubst mode-line-is-active+ ()
  "Return non-nil if the current window is active."
  (eq (selected-window) mode-line--current-window+))

(defun mode-line-format+ (left right)
  "Return a string of `window-width' length containing LEFT, and RIGHT
 aligned respectively."
  (let* ((available-width (- (window-total-width) (length left) 2)))
    (format (format "%%s %%%ds " available-width) left right)))

;; * Update functions

;; VC update function
(defvar-local mode-line--vc-text+ nil)
(defun mode-line--update-vc-segment+ (&rest _)
  "Update `mode-line--vc-text+' against the current VCS state."
  (setq mode-line--vc-text+
        (when (and vc-mode buffer-file-name)
          (let ((backend (vc-backend buffer-file-name))
                (state (vc-state buffer-file-name (vc-backend buffer-file-name))))
            (let ((face 'mode-line-inactive)
                  (active (mode-line-is-active+)))
              (propertize (substring vc-mode (+ (if (eq backend 'Hg) 2 3) 2))
                          'face (if active face)))))))


;; * Segments

(defvar all-the-icons-scale-factor)
(defun mode-line-segment-buffer-name+ ()
  "Displays the name of the current buffer in the mode-line."
  (concat " "
          (cond ((eq major-mode 'shell-mode)
                 (shorten-directory+ default-directory 20))
                (t (propertize "%b"
                               'mouse-face 'mode-line-highlight
                               'local-map (make-mode-line-mouse-map
                                           'mouse-1
                                           'dired-here-please))))))

(defun mode-line-segment-buffer-modified+ ()
  (when (and (buffer-modified-p)
             (buffer-file-name))
    (propertize " (!)"
                'face 'bold
                'mouse-face 'mode-line-highlight
                'local-map (make-mode-line-mouse-map
                            'mouse-1
                            'save-buffer))))

(defun mode-line-segment-mode-icon+ ()
  (if (eq major-mode 'fundamental-mode)
      (all-the-icons-octicon
       ;; dash
          "question"
          :height (/ all-the-icons-scale-factor 1.4)
          :v-adjust 0.0)
    (all-the-icons-icon-for-mode
     major-mode
     :height (/ all-the-icons-scale-factor 1.4)
     :v-adjust (if (derived-mode-p 'emacs-lisp-mode)
                   -0.1 0.0))))

(defun mode-line-segment-evil-state-icon+ ()
  (propertize
   (concat (all-the-icons-faicon "circle" :height (/ all-the-icons-scale-factor 1.4) :v-adjust 0.0))
   'face (cond
    ((eq evil-state 'normal) 'mode-line-evil-state-normal)
    ((eq evil-state 'insert) 'mode-line-evil-state-insert)
    ((eq evil-state 'visual) 'mode-line-evil-state-visual)
    ((eq evil-state 'motion) 'mode-line-evil-state-motion)
    ((eq evil-state 'emacs) 'mode-line-evil-state-emacs)
    (t ""))))

(defun mode-line-segment-position+ ()
  "Displays the current cursor position in the mode-line."
  ;; %C for column 1 based
  (let ((col (format-mode-line "%C"))
        (line (format-mode-line "%l")))
    ;; comint modes fail to display it
    (if (string= line "??")
        (setq line (number-to-string (line-number-at-pos (point)))))
    (concat
     line
     ":"
     ;; avoid bumping in mode line
     (if (< (length col) 2)
         "0" "")
     col
     "  %p%%"
     )))

(defun mode-line-segment-vc+ ()
  "Displays color-coded version control information in the mode-line."
  mode-line--vc-text+)

(defun mode-line-segment-flycheck+ ()
  "Return the status of flycheck to be displayed in the mode-line."
  (when (bound-and-true-p flycheck-mode)
    (let* ((text
            (pcase flycheck-last-status-change
              (`finished
               (if flycheck-current-errors
                   (let ((count (let-alist (flycheck-count-errors
                                            flycheck-current-errors)
                                  (+ (or .warning 0) (or .error 0)))))
                     (propertize
                      (format "✖ %s Issue%s" count (if (eq 1 count) "" "s"))
                      'face (mode-line-face-if-active+ 'font-lock-keyword-face)))
                 (propertize
                  "✔ No Issues"
                  'face (mode-line-face-if-active+ 'success))))
              (`running
               (propertize
                "⟲ Running"
                'face (mode-line-face-if-active+ 'warning)))
              (`no-checker
               (propertize
                "✖ No Checker"
                'face (mode-line-face-if-active+ 'warning)))
              (`not-checked
               "● Disabled")
              (`errored (propertize
                         "⚠ Error"
                         'face
                         (mode-line-face-if-active+ 'error)))
              (`interrupted
               (propertize
                "⛔ Interrupted"
                'face (mode-line-face-if-active+ 'error)))
              (`suspicious  ""))))
      (propertize text
                  'help-echo "Show Flycheck Errors"
                  'local-map (make-mode-line-mouse-map
                              'mouse-1 #'flycheck-list-errors)))))




(defun mode-line-segment-front-space+ ()
  "Setup mode line padding."
  ;; (if (zerodark--active-window-p)
  ;; ;;(list :line-width 4 :color "grey90"))
  ;; :box
  (if (and (not (window-dedicated-p))
           (display-graphic-p))
      (propertize " " 'display
                  '((raise -0.2) (height 1.4)))
    mode-line-front-space))

;; * Main

;; (moody-replace-vc-mode)

(defvar-local mode-line-remap-cookies+ nil)
(defvar-local window-side+ nil
  "Record the side of window showing buffer.")


(defun mode-line-color+ ()
  (prog1 nil
    (if (window-dedicated-p)
        (setq mode-line-remap-cookies+
              (list (face-remap-add-relative 'mode-line
                                             '(:background "seashell"))
                    (face-remap-add-relative 'mode-line-inactive
                                             '(:background "seashell")))
              window-side+ (window-parameter nil 'window-side))
      (mapcar #'face-remap-remove-relative mode-line-remap-cookies+))))

(defvar mode-line--saved-default+ nil)

(setq mode-line-modes
  (let ((recursive-edit-help-echo "Recursive edit, type C-M-c to get out"))
    (list (propertize "%[" 'help-echo recursive-edit-help-echo)
	  `(:propertize ("" mode-name)
			help-echo "Major mode\n\
mouse-1: Display major mode menu\n\
mouse-2: Show help for major mode\n\
mouse-3: Toggle minor modes"
			mouse-face mode-line-highlight
			local-map ,mode-line-major-mode-keymap)
	  '("" mode-line-process)
	  `(:propertize ("" minor-mode-alist)
			mouse-face mode-line-highlight
			help-echo "Minor mode\n\
mouse-1: Display minor mode menu\n\
mouse-2: Show help for minor mode\n\
mouse-3: Toggle minor modes"
			local-map ,mode-line-minor-mode-keymap)
	  (propertize "%n" 'help-echo "mouse-2: Remove narrowing from buffer"
		      'mouse-face 'mode-line-highlight
		      'local-map (make-mode-line-mouse-map
				  'mouse-2 #'mode-line-widen))
	  (propertize "%]" 'help-echo recursive-edit-help-echo)
	  " ")))

(defvar mode-line-format+
  '(""
    (:eval (mode-line-color+))
    (:eval
     (mode-line-format+
      ;; Left
      (format-mode-line
       '("%e"
         (:eval (mode-line-segment-front-space+))
         ;; (:eval (mode-line-segment-mode-icon+))
         (:eval (mode-line-segment-evil-state-icon+))
         (:eval (mode-line-segment-buffer-name+))
         (:eval (mode-line-segment-buffer-modified+))
         " | "
         mode-line-modes
         ))
      ;; Right
      (format-mode-line
       '((:eval (mode-line-segment-vc+))
         (:eval (mode-line-segment-flycheck+))
         " "
         mode-line-misc-info
         (:eval (mode-line-segment-position+))
         mode-line-end-spaces))))))

;;;###autoload
(define-minor-mode mode-line-mode+
  "Replace the current mode-line with mode-line.+"
  :global t
  :lighter ""
  (cond (mode-line-mode+
     ;; Save old format
     (setq mode-line--saved-default+ (default-value 'mode-line-format))
     ;; Set the new mode-line-format
     (setq-default mode-line-format mode-line-format+)
     (dolist (buf (buffer-list))
       (setf (buffer-local-value 'mode-line-format (current-buffer))
         mode-line-format+))
     ;; Setup VC hooks (if available)
     (add-hook 'find-file-hook #'mode-line--update-vc-segment+)
     (add-hook 'after-save-hook #'mode-line--update-vc-segment+)
     (advice-add #'vc-refresh-state :after #'mode-line--update-vc-segment+)
     ;; Setup remembering active window
     (add-hook 'window-configuration-change-hook #'mode-line--update-selected-window+)
     (add-hook 'focus-in-hook #'mode-line--update-selected-window+)
     (advice-add #'handle-switch-frame :after #'mode-line--update-selected-window+)
     (advice-add #'select-window :after #'mode-line--update-selected-window+))
    (t
     ;; Set the old mode-line-format
     (setq-default mode-line-format mode-line--saved-default+)
     (dolist (buf (buffer-list))
       (setf (buffer-local-value 'mode-line-format (current-buffer))
         (default-value 'mode-line-format)))
     ;; Setup flycheck hooks (if available)
     ;;(remove-hook 'flycheck-status-changed-functions #'mode-line-segment-flycheck+)
     ;;(remove-hook 'flycheck-mode-hook #'mode-line-segment-flycheck+)
     ;; Setup VC hooks (if available)
     (remove-hook 'find-file-hook #'mode-line--update-vc-segment+)
     (remove-hook 'after-save-hook #'mode-line--update-vc-segment+)
     (advice-remove #'vc-refresh-state #'mode-line--update-vc-segment+)
     ;; Setup remembering active window
     (remove-hook 'window-configuration-change-hook #'mode-line--update-selected-window+)
     (remove-hook 'focus-in-hook #'mode-line--update-selected-window+)
     (advice-remove #'handle-switch-frame #'mode-line--update-selected-window+)
     (advice-remove #'select-window  #'mode-line--update-selected-window+))))



(provide 'mode-line+)

;;; mode-line+.el ends here
