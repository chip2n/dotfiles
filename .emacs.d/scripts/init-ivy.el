(provide 'init-ivy)

(use-package ivy
  :ensure t
  :config
  (ivy-mode)
  ;; slim down ivy display
  (setq ivy-count-format ""
        ivy-display-style nil
        ivy-minibuffer-faces nil))

(use-package ivy-rich
  :ensure t
  :config
  (setq ivy-rich-display-transformers-list
        '(counsel-find-file
          (:columns
           ((ivy-read-file-transformer)
            (ivy-rich-counsel-find-file-truename
             (:face font-lock-comment-face))))
          counsel-M-x
          (:columns
           ((counsel-M-x-transformer
             (:width 40))
            (ivy-rich-counsel-function-docstring
             (:face font-lock-comment-face))))
          counsel-describe-function
          (:columns
           ((counsel-describe-function-transformer
             (:width 40))
            (ivy-rich-counsel-function-docstring
             (:face font-lock-comment-face))))
          counsel-describe-variable
          (:columns
           ((counsel-describe-variable-transformer
             (:width 40))
            (ivy-rich-counsel-variable-docstring
             (:align right :face font-lock-comment-face))))
          package-install
          (:columns
           ((ivy-rich-candidate
             (:width 30))
            (ivy-rich-package-version
             (:width 16 :face font-lock-comment-face))
            (ivy-rich-package-archive-summary
             (:width 7 :face font-lock-comment-face))
            (ivy-rich-package-install-summary
             (:face font-lock-comment-face))))))
  (ivy-rich-mode 1))

(use-package counsel
  :ensure t
  :after (ivy)
  :config
  (counsel-mode))

;; more intelligent order when using counsel-M-x
(use-package smex
  :ensure t
  :config
  :after (counsel)
  (smex-initialize))

(use-package swiper
  :ensure t
  :after (ivy))

;; (defun ivy-posframe-display-at-frame-bottom-center (str)
;;   (ivy-posframe--display str #'posframe-poshandler-frame-bottom-center))

;; (use-package ivy-posframe
;;   :ensure t
;;   :config
;;   (setq ivy-posframe-display-functions-alist
;;         '((t . ivy-posframe-display-at-frame-bottom-center)))
;;   (setq ivy-posframe-parameters
;;         '((left-fringe . 0)
;;           (right-fringe . 0)))
;;   (ivy-posframe-mode 0))
