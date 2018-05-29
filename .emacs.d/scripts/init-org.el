(provide 'init-org)

;; add timestamp to completed todos
(setq org-log-done 'time)

;; prevent indentation after sections
(setq org-adapt-indentation nil)

;; add org directory (allows searching for todos and scheduling items)
(setq org-agenda-files (list "~/org/personal" "~/org/remente"))

;; set org tag column
(setq org-tags-column -80)

;; open links in same window
;(setq org-link-frame-setup (file . find-file))

(require 'org-drill)

(use-package evil-org
   :ensure t
   :after org
   :config
   (add-hook 'org-mode-hook 'evil-org-mode)
   (add-hook 'evil-org-mode-hook
             (lambda ()
               (evil-org-set-key-theme)))
   (setq org-file-apps
         '(("\\.png\\'" . "feh --scale-down \"%s\"")
           ("\\.jpg\\'" . "feh --scale-down \"%s\"")
           (auto-mode . emacs)
           ("\\.mm\\'" . default)
           ("\\.x?html?\\'" . default)
           ("\\.pdf\\'" . default)))
   (setq org-ellipsis "  ")
   (setq org-startup-indented t))
