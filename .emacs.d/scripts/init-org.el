(provide 'init-org)

(use-package org-bullets
  :ensure t)

;; add timestamp to completed todos
(setq org-log-done 'time)

;; prevent indentation after sections
(setq org-adapt-indentation nil)

;; add org directory (allows searching for todos and scheduling items)
(setq org-agenda-files (list "~/org/personal" "~/org/remente"))

;; set org tag column
(setq org-tags-column -80)

;; show fully expanded items when using org-tags-sparse-tree
(push '(tags-tree . local) org-show-context-detail)

(setq org-capture-templates
      '(("a" "My TODO task format." entry
	 (file "todo.org")
	 "* TODO %?
SCHEDULED: %t")))

;; hydra for org-mode shortcuts
(defhydra chip-hydra-org-shortcuts
    (:hint nil
     :color amaranth
     :exit t)
"
   Nav^            ^Quit
 ╭─^──^───────────^───^─────╮
   [_a_] agenda     [_q_] quit
   [_t_] todos
 ╰─^──^───────────^───^─────╯
"
  ("a" org-agenda)
  ("t" org-todo-list)
  ("q" nil))
(evil-leader/set-key
  "o" 'chip-hydra-org-shortcuts/body)

;; hydra for org-mode interaction
(defhydra chip-hydra-org-mode
    (:color amaranth
     :hint nil)
"
    Nav
 ╭─────────────────────────────────────────────────────╮
   ^ ^ _k_ ^ ^
       ^+^
   ^ ^ _j_ ^ ^
 ╰─────────────────────────────────────────────────────╯
"
  ("j" outline-next-visible-heading)
  ("k" outline-previous-visible-heading)
  ("h" org-backward-heading-same-level)
  ("l" org-forward-heading-same-level)
  ("K" outline-up-heading)
  ("<tab>" org-cycle)
  ("t" org-todo)
  ("q" nil))

;; hydra for org-agenda interaction
(defhydra chip-hydra-org-agenda-mode
    (:hint nil
     :color amaranth)
"
   Nav^   ^Actions
 ╭─^──^───^───^────────╮
    _k_    [_t_] toggle
    ^↕^    [_q_] quit
    _j_
 ╰─^──^───^───^────────╯
"
  ("j" org-agenda-next-line)
  ("k" org-agenda-previous-line)
  ("t" org-agenda-todo)
  ("q" org-agenda-exit :exit t))
;;(add-hook 'org-agenda-mode-hook 'chip-hydra-org-agenda-mode/body)

;; remove font height differences for headers
(custom-set-faces
  '(org-level-1 ((t (:inherit outline-1 :height 1.0))))
  '(org-level-2 ((t (:inherit outline-2 :height 1.0))))
  '(org-level-3 ((t (:inherit outline-3 :height 1.0))))
  '(org-level-4 ((t (:inherit outline-4 :height 1.0))))
  '(org-level-5 ((t (:inherit outline-5 :height 1.0))))
)

(require 'org-agenda)
(define-key org-agenda-mode-map "j" 'org-agenda-next-line)
(define-key org-agenda-mode-map "k" 'org-agenda-previous-line)
(define-key org-agenda-mode-map (kbd "RET") 'org-agenda-cycle-show)
(define-key org-agenda-mode-map (kbd "C-h") 'windmove-left)
(define-key org-agenda-mode-map (kbd "C-l") 'windmove-right)
(define-key org-agenda-mode-map (kbd "C-j") 'windmove-down)
(define-key org-agenda-mode-map (kbd "C-k") 'windmove-up)
(define-key org-agenda-mode-map (kbd "C-S-l") 'evil-window-increase-width)
(define-key org-agenda-mode-map (kbd "C-S-h") 'evil-window-decrease-width)
(define-key org-agenda-mode-map (kbd "C-S-k") 'evil-window-increase-height)
(define-key org-agenda-mode-map (kbd "C-S-j") 'evil-window-decrease-height)
