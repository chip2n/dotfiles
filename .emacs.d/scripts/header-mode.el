;;; header-mode.el --- Add fancy header showing filename / buffer name
;;; Code:

(defvar header-icon nil)
(defvar header-format-filepath 'chip/format-header-filepath)
(defvar header-format-buffer 'chip/format-header-buffer)

(defun chip/show-header ()
  (interactive)
  (progn
    (sl/display-header)
    (set-face-attribute 'header-line nil :box '(:line-width 8 :color "#21242b"))
    (set-face-background 'header-line "#21242b")))

(defun chip/hide-header ()
  (interactive)
  (setq header-line-format nil))

(defun chip/update-header ()
  (interactive)
  (if (or (bound-and-true-p writeroom-mode)
          (eq major-mode 'exwm-mode)
          (eq major-mode 'inf-clojure-mode))
      (chip/hide-header)
    (chip/show-header)))

(defmacro with-face (str &rest properties)
  `(propertize ,str 'face (list ,@properties)))

;; TODO use same color as chip-theme
(defun sl/make-header ()
  (let* ((sl/full-header (abbreviate-file-name buffer-file-name))
         (sl/header (file-name-directory sl/full-header))
         (sl/drop-str "[...] "))
    (if (> (length sl/full-header)
           (window-body-width))
        (if (> (length sl/header)
               (window-body-width))
            (progn
              (concat (with-face sl/drop-str
                                 :foreground "#494e5a"
                                 :weight 'bold
                                 )
                      (with-face (substring sl/header
                                            (+ (- (length sl/header)
                                                  (window-body-width))
                                               (length sl/drop-str))
                                            (length sl/header))
                                 :weight 'bold
                                 )))
          (concat (with-face sl/header
                             :foreground "#b9e59f"
                             :weight 'bold
                             )))
      (concat (with-face sl/header
                         :weight 'bold
                         :foreground "#b9e59f"
                         )
              (with-face (file-name-nondirectory buffer-file-name)
                         :weight 'bold
                         )))))

(defun sl/display-header ()
  (setq header-line-format
        '("" ;; invocation-name
          (:eval (if (buffer-file-name)
                     (chip/create-header-filepath)
                     (chip/create-header-buffer))))))

(defun chip/create-header-filepath ()
  (funcall header-format-filepath (sl/make-header)))

(defun chip/create-header-buffer ()
  (funcall header-format-buffer " %b"))

(defun chip/format-header-filepath (header)
  (concat " " header-icon " " header))

(defun chip/format-header-buffer (buffer)
  (concat " " header-icon buffer))

(define-minor-mode header-mode
  "Add fancy header showing filename / buffer name."
  :global nil
  (if header-mode
      (chip/update-header)
    (chip/hide-header)))

(provide 'header-mode)

;;; header-mode.el ends here
