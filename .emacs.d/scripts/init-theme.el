(provide 'init-theme)

;; margin stuff
(set-frame-parameter nil 'internal-border-width 0)

;; highlight the current line
(global-hl-line-mode +1)

;; all-the-icons
(use-package all-the-icons
  :ensure t)

;; hide welcome screen
(setq inhibit-startup-screen t)

;; doom theme
(use-package doom-themes
  :ensure t)
(load-theme 'doom-one t)
(setq doom-enable-bold t
      doom-enable-italic t)
;;(require 'doom-neotree)
(setq neo-theme (if (display-graphic-p) 'icons 'arrow))

;; telephone-line (https://github.com/dbordak/telephone-line)
(use-package telephone-line
  :ensure t)
(setq telephone-line-lhs
        '((evil   . (telephone-line-evil-tag-segment))
          (accent . (telephone-line-vc-segment
                     telephone-line-process-segment))
          (nil    . (telephone-line-minor-mode-segment))
	  ))
(setq telephone-line-rhs
        '((nil    . (telephone-line-misc-info-segment))
          (accent . (telephone-line-major-mode-segment))
          (evil   . (telephone-line-airline-position-segment))))

(telephone-line-mode t)

;; set default font
;;(set-face-attribute 'default nil
;;                    :family "terminus"
;;                    :height 100)
(set-default-font "-*-terminus-*-*-*-*-14-*-*-*-*-*-*-*")

;; disable gui fluff
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; disable blinking cursor (caused some flicker in images when using eww)
(blink-cursor-mode 0)

(set-cursor-color "#ffffff")
(set-background-color "#21242b")
(set-face-background hl-line-face "#1c1f24")





(defun chip/show-header ()
  ""
  (interactive)
  (progn
    (sl/display-header)
    (set-face-attribute 'header-line nil :box '(:line-width 8 :color "#21242b"))
    (set-face-background 'header-line "#21242b")
    ))

(defun chip/hide-header ()
  ""
  (interactive)
  (setq header-line-format nil))

(defun chip/update-header ()
  ""
  (interactive)
  (if (buffer-file-name)
      (chip/show-header)
    (chip/hide-header)))




;; Display file paths in header
(defmacro with-face (str &rest properties)
  `(propertize ,str 'face (list ,@properties)))

(defun sl/make-header ()
  ""
  (let* ((sl/full-header (abbreviate-file-name buffer-file-name))
         (sl/header (file-name-directory sl/full-header))
         (sl/drop-str "[...]"))
    (if (> (length sl/full-header)
           (window-body-width))
        (if (> (length sl/header)
               (window-body-width))
            (progn
              (concat (with-face sl/drop-str
                                 :background "blue"
                                 :weight 'bold
                                 )
                      (with-face (substring sl/header
                                            (+ (- (length sl/header)
                                                  (window-body-width))
                                               (length sl/drop-str))
                                            (length sl/header))
                                 ;; :background "red"
                                 :weight 'bold
                                 )))
          (concat (with-face sl/header
                             ;; :background "red"
                             :foreground "#98be65"
                             :weight 'bold
                             )))
      (concat (with-face sl/header
                         ;; :background "green"
                         ;; :foreground "black"
                         :weight 'bold
                         :foreground "#98be65"
                         )
              (with-face (file-name-nondirectory buffer-file-name)
                         :weight 'bold
                         ;; :background "red"
                         )))))

(defun sl/display-header ()
  (setq header-line-format
        '("" ;; invocation-name
          (:eval (if (buffer-file-name)
                     (concat " " (sl/make-header))
                   "%b")))))



(add-hook 'buffer-list-update-hook
          'chip/update-header)
