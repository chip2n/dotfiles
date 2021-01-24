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
  :config
  (selectrum-mode +1))

(use-package selectrum-prescient
  :after (prescient selectrum)
  :config
  (selectrum-prescient-mode +1))

;; Prescient allows you to filter and automatically sort ivy and company results
;; by frequency. It also enables searching by initialism (e.g. stbow ->
;; switch-to-buffer-other-window).
(use-package prescient
  :config
  (prescient-persist-mode))

(provide 'chip-completion)
