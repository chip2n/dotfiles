(use-package embark
  :bind ("C-," . embark-act)
  :config
  ;; show command help via which-key
  (setq embark-action-indicator
        (lambda (map)
          (which-key--show-keymap "Embark" map nil nil 'no-paging)
          #'which-key--hide-popup-ignore-command)
        embark-become-indicator embark-action-indicator)

  (embark-define-keymap embark-project-file-map
    "Keymap for actions when completing on project files."
    ("d" delete-file)
    ;; ("r" rename-file)
    ;; ("c" copy-file)
    ))

(use-package consult
  :config
  ;; Configure a function which returns the project root directory
  (autoload 'projectile-project-root "projectile")
  (setq consult-project-root-function #'projectile-project-root))

(use-package marginalia
  :init
  ;; Must be in the :init section of use-package such that the mode gets
  ;; enabled right away. Note that this forces loading the package.
  (marginalia-mode)

  ;; Display more annotations - e.g. docstring with M-x
  (setq marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))

  ; When using Selectrum, ensure that Selectrum is refreshed when cycling annotations.
  (advice-add #'marginalia-cycle :after
              (lambda () (when (bound-and-true-p selectrum-mode) (selectrum-exhibit)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :ensure t
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . embark-consult-preview-minor-mode))

(use-package selectrum
  :bind (:map selectrum-minibuffer-map
         (("<next>" . 'selectrum-next-page)
          ("<prior>" . 'selectrum-previous-page)))
  :config
  ;; I want the candidate highligt background to extend to the edge of the frame
  (setq selectrum-extend-current-candidate-highlight nil)

  (selectrum-mode +1))

;; Prescient allows you to filter and automatically sort ivy and company results
;; by frequency. It also enables searching by initialism (e.g. stbow ->
;; switch-to-buffer-other-window).
(use-package prescient
  :config
  (prescient-persist-mode))

(use-package selectrum-prescient
  :after (prescient selectrum)
  :config
  (selectrum-prescient-mode +1))

(use-package company-prescient
  :after (company)
  :config
  (company-prescient-mode))

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
