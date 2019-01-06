(provide 'init-theme)

;; load main emacs theme
(load-theme 'chip t)

;; margin stuff
;;(set-frame-parameter nil 'internal-border-width 10)

;; disable gui fluff
(scroll-bar-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)

;; show line numbers
(global-display-line-numbers-mode t)

;; hide welcome screen
(setq inhibit-startup-screen t)

;; highlight the current line
(global-hl-line-mode +1)

;; setup modeline
(use-package telephone-line
  :ensure t
  :after (evil)
  :config
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
  
  (telephone-line-mode t))

;; set font
;; (set-frame-font "-*-terminus-*-*-*-*-14-*-*-*-*-*-*-*" t t)
;; (set-frame-font "Hack 11" nil t)
(set-face-attribute 'default nil
        :family "Hack"
        :height 100)

;; Change fringe icons
(define-fringe-bitmap 'left-curly-arrow
  [#b11011000
   #b01101100
   #b00110110
   #b00011011
   #b00110110
   #b01101100
   #b11011000])
(define-fringe-bitmap 'right-curly-arrow
  [#b00011011
   #b00110110
   #b01101100
   #b11011000
   #b01101100
   #b00110110
   #b00011011])



(use-package all-the-icons
  :ensure t)

(defun chip/show-header ()
  ""
  (interactive)
  (progn
    (sl/display-header)
    (set-face-attribute 'header-line nil :box '(:line-width 8 :color "#21242b"))
    (set-face-background 'header-line "#21242b")
    ;(set-face-background 'header-line "#282c34")
    ))

(defun chip/hide-header ()
  ""
  (interactive)
  (setq header-line-format nil))

(defun chip/update-header ()
  ""
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
  ""
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
                                 ;; :background "red"
                                 :weight 'bold
                                 )))
          (concat (with-face sl/header
                             ;; :background "red"
                             :foreground "#b9e59f"
                             :weight 'bold
                             )))
      (concat (with-face sl/header
                         ;; :background "green"
                         ;; :foreground "black"
                         :weight 'bold
                         :foreground "#b9e59f"
                         )
              (with-face (file-name-nondirectory buffer-file-name)
                         :weight 'bold
                         ;; :background "red"
                         )))))

(defun sl/display-header ()
  (setq header-line-format
        '("" ;; invocation-name
          (:eval (if (buffer-file-name)
                     (chip/create-header-filepath)
                     (chip/create-header-buffer))))))

(defun chip/create-header-filepath ()
  ;(concat (with-face "g" :background "#ff0000")
  (concat " "
          (concat (chip/header-icon)
                  (concat " " (sl/make-header)))))

(defun chip/create-header-buffer ()
  (concat " "
          (concat (chip/header-icon)
                  " %b")))

(defun chip/create-header (title)
  (concat " " (concat (chip/header-icon) " " title)))

(defun chip/header-icon ()
  (all-the-icons-faicon "codepen" :height 0.8 :v-adjust 0.1))

(add-hook 'buffer-list-update-hook 'chip/update-header)
