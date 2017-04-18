(provide 'init-org)

;; add timestamp to completed todos
(setq org-log-done 'time)

;; add org directory (allows searching for todos and scheduling items)
(setq org-agenda-files (list "~/org/personal" "~/org/remente"))

(setq org-capture-templates
      '(("a" "My TODO task format." entry
	 (file "todo.org")
	 "* TODO %?
SCHEDULED: %t")))


;; remove font height differences for headers
(custom-set-faces
  '(org-level-1 ((t (:inherit outline-1 :height 1.0))))
  '(org-level-2 ((t (:inherit outline-2 :height 1.0))))
  '(org-level-3 ((t (:inherit outline-3 :height 1.0))))
  '(org-level-4 ((t (:inherit outline-4 :height 1.0))))
  '(org-level-5 ((t (:inherit outline-5 :height 1.0))))
)
