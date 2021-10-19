(defun embark-which-key-indicator ()
  "An embark indicator that displays keymaps using which-key.
The which-key help message will show the type and value of the
current target followed by an ellipsis if there are further
targets."
  (lambda (&optional keymap targets prefix)
    (if (null keymap)
        (which-key--hide-popup-ignore-command)
      (which-key--show-keymap
       (if (eq (caar targets) 'embark-become)
           "Become"
         (format "Act on %s '%s'%s"
                 (plist-get (car targets) :type)
                 (embark--truncate-target (plist-get (car targets) :target))
                 (if (cdr targets) "…" "")))
       (if prefix
           (pcase (lookup-key keymap prefix 'accept-default)
             ((and (pred keymapp) km) km)
             (_ (key-binding prefix 'accept-default)))
         keymap)
       nil nil t))))

(defun c/embark-file-open-term (path)
  (let* ((path (file-name-directory path))
         (default-directory path))
    (c/vterm-toggle-cd path)))

(defun c/embark-project-open-term (path)
  (c/embark-file-open-term
   (expand-file-name path (projectile-project-root))))

(defun c/embark-file-save-absolute-path (file)
  "Save the absolute path to FILE in the kill ring"
  (interactive "FFile: ")
  (kill-new (expand-file-name file default-directory)))

(defun c/embark-project-save-absolute-path (file)
  "Save the absolute path to FILE in the kill ring"
  (interactive "FFile: ")
  (kill-new (expand-file-name file (projectile-project-root))))

(use-package embark
  :bind ("C-," . embark-act)
  :config
  ;; show command help via which-key
  (setq embark-indicators
  '(embark-which-key-indicator
    embark-highlight-indicator
    embark-isearch-highlight-indicator))

  (embark-define-keymap embark-project-map
    "Keymap for actions when completing on project files.")

  (define-key embark-file-map (kbd "t") 'c/embark-file-open-term)
  (define-key embark-file-map (kbd "a") 'c/embark-file-save-absolute-path)

  (define-key embark-project-map (kbd "t") 'c/embark-project-open-term)
  (define-key embark-project-map (kbd "a") 'c/embark-project-save-absolute-path)

  (add-to-list 'embark-keymap-alist '(project . embark-project-map))
  (add-to-list 'marginalia-command-categories '(projectile-find-file . project))
  (add-to-list 'marginalia-command-categories '(projectile-find-dir . project))
  (add-to-list 'marginalia-command-categories '(projectile-switch-project . project))

  (add-to-list 'marginalia-command-categories '(projectile-commander . project)))

(use-package embark-consult
  :ensure t
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . embark-consult-preview-minor-mode))

(use-package consult
  :config
  ;; Configure a function which returns the project root directory
  (autoload 'projectile-project-root "projectile")
  (setq consult-project-root-function #'projectile-project-root)

  (after-load (evil)
    ;; Remember point before search for `evil-jump-backward'
    (evil-set-command-property 'consult-line :jump t)))

(use-package marginalia
  :init
  ;; Must be in the :init section of use-package such that the mode gets
  ;; enabled right away. Note that this forces loading the package.
  (marginalia-mode)

  ;; Display more annotations - e.g. docstring with M-x
  (setq marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))

  ;; When using Selectrum, ensure that Selectrum is refreshed when cycling annotations.
  (advice-add #'marginalia-cycle :after
              (lambda () (when (bound-and-true-p selectrum-mode) (selectrum-exhibit)))))

;; -----------------------------------------------------------------------------
;; Marginalia doesn't remember the this-command when switching projects using
;; projectile, since it uses multiple minibuffers. In order to classify project
;; completions properly, we keep track of when we're in the process of switching
;; projects and make sure to return the correct category

(defvar c/switching-project? nil)
(defun c/projectile-before-switch-project ()
  (setq c/switching-project? t))
(defun c/projectile-after-switch-project ()
  (setq c/switching-project? nil))

(after-load (projectile marginalia)
  (add-hook 'projectile-before-switch-project-hook #'c/projectile-before-switch-project)
  (add-hook 'projectile-after-switch-project-hook #'c/projectile-after-switch-project)

  (advice-add 'marginalia-classify-by-prompt :around
              (lambda (orig-fun &rest args)
                (if c/switching-project?
                    'project
                  (apply orig-fun args)))))
;; -----------------------------------------------------------------------------

(use-package orderless
  :ensure t
  :custom (completion-styles '(basic partial-completion orderless))
  :config
  (setq orderless-matching-styles '(orderless-literal orderless-regexp))
  ;; allow & as separator, useful for company-mode where space breaks completion
  (setq orderless-component-separator "[ &]")
  (after-load (selectrum)
    (setq orderless-skip-highlighting (lambda () selectrum-is-active)))
  (savehist-mode 1))

(defun c/selectrum-kill-backwards (&optional arg)
  "Custom code for killing backwards inside selectrum without saving to the kill ring.
ARG is the same as for `backward-kill-sexp'."
  (interactive "p")
  (save-restriction
    (narrow-to-region (minibuffer-prompt-end) (point-max))
    (let ((opoint (point)))
      (forward-sexp (- (or arg 1)))
      (delete-region opoint (point)))))

(use-package selectrum
  :bind (:map selectrum-minibuffer-map
         (("<next>" . 'selectrum-next-page)
          ("<prior>" . 'selectrum-previous-page)
          ("C-<backspace>" . 'c/selectrum-kill-backwards)))
  :config
  ;; I want the candidate highligt background to extend to the edge of the frame
  (setq selectrum-extend-current-candidate-highlight nil)

  ;; Got some issues with buffer height when using 1440p monitor - this seems to fix that
  (setq selectrum-fix-vertical-window-height t)

  (after-load (orderless)
    (setq selectrum-highlight-candidates-function #'orderless-highlight-matches))

  (selectrum-mode +1))

(define-minor-mode c/complete-mode
  "Enable code completion."
  :global nil
  (if c/complete-mode
      (progn
        (company-mode 1))
    (company-mode 0)))

(add-hook 'prog-mode-hook 'c/complete-mode)

(defun chip/company-setup-keys ()
  "Setup keybindings for company mode"
  (interactive)
  (define-key company-active-map (kbd "<return>") nil)
  (define-key company-active-map (kbd "RET") nil)
  (define-key company-active-map (kbd "<tab>") #'company-complete-selection))

(use-package company
  :config
  (c/diminish company-mode)

  (general-define-key
   :keymap 'prog-mode-map
   "M-/" 'counsel-company)
  (add-hook 'company-mode-hook 'chip/company-setup-keys)
  ;; prevent downcasing when autocompleting
  (setq company-dabbrev-downcase nil)
  (setq evil-complete-next-func 'complete-complete-cycle-next)
  (setq evil-complete-previous-func 'complete-complete-cycle-previous)

  ;; show company completion with delay
  (setq company-idle-delay 0.3)

  ;; show suggestions after entering one character.
  (setq company-minimum-prefix-length 1)

  (setq company-selection-wrap-around t)

  ;; aligns annotation to the right hand side
  (setq company-tooltip-align-annotations t))

(defun complete-complete-cycle-next (arg)
  (company-complete-common-or-cycle))

(defun complete-complete-cycle-previous (arg)
  (company-complete-common-or-cycle -1))

(use-package company-box
  :after (company)
  :hook (company-mode . company-box-mode)
  :config
  (c/diminish company-box-mode))

;;; Ivy configuration (disabled)

;; (use-package ivy
;;   :config
;;   (ivy-mode)
;;   ;; slim down ivy display
;;   (setq ivy-count-format ""
;;         ivy-display-style nil
;;         ivy-minibuffer-faces nil)

;;   (define-key ivy-minibuffer-map (kbd "<escape>") 'minibuffer-keyboard-quit))

;; (use-package ivy-rich
;;   :config
;;   (setq ivy-rich-display-transformers-list
;;         '(counsel-find-file
;;           (:columns
;;            ((ivy-read-file-transformer)
;;             (ivy-rich-counsel-find-file-truename
;;              (:face font-lock-comment-face))))
;;           counsel-M-x
;;           (:columns
;;            ((counsel-M-x-transformer
;;              (:width 40))
;;             (ivy-rich-counsel-function-docstring
;;              (:face font-lock-comment-face))))
;;           counsel-describe-function
;;           (:columns
;;            ((counsel-describe-function-transformer
;;              (:width 40))
;;             (ivy-rich-counsel-function-docstring
;;              (:face font-lock-comment-face))))
;;           counsel-describe-variable
;;           (:columns
;;            ((counsel-describe-variable-transformer
;;              (:width 40))
;;             (ivy-rich-counsel-variable-docstring
;;              (:align right :face font-lock-comment-face))))
;;           package-install
;;           (:columns
;;            ((ivy-rich-candidate
;;              (:width 30))
;;             (ivy-rich-package-version
;;              (:width 16 :face font-lock-comment-face))
;;             (ivy-rich-package-archive-summary
;;              (:width 7 :face font-lock-comment-face))
;;             (ivy-rich-package-install-summary
;;              (:face font-lock-comment-face))))))
;;   (ivy-rich-mode 1))

;; (use-package counsel
;;   :after (ivy))

;; (use-package swiper
;;   :after (ivy))

;; (use-package ivy-prescient
;;   :after (counsel)
;;   :config
;;   (ivy-prescient-mode))

(provide 'chip-completion)
