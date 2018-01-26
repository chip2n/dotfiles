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
