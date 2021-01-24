(general-define-key
     :prefix "C-c"
     ;; org-mode agenda
     "a" 'chip/org-agenda

     ;; org-mode capture
     "e" 'org-capture

     ;; org-mode clocking
     "o i" 'org-clock-in
     "o o" 'org-clock-out
     "o g" 'org-clock-goto

     ;; quickly jump to a task in your agenda files
     "j" 'counsel-org-agenda-headlines)

(provide 'chip-keys)
