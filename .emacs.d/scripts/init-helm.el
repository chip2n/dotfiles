(provide 'init-helm)

(use-package helm
  :ensure t)
(helm-mode 1)
(evil-leader/set-key
  "<SPC>" 'helm-M-x
  "b" 'helm-buffers-list)

;; projectile
(use-package projectile
  :ensure t)
(projectile-mode)
(use-package helm-projectile
  :ensure t)
(evil-leader/set-key
  "p" 'helm-projectile)
(use-package helm-ag
  :ensure t)

;; vim movement in helm
(defun helm-like-unite/pre ()
  (set-cursor-color "#ffffff"))
(defun helm-like-unite/post ()
  (set-cursor-color "#ffffff"))
(defhydra helm-like-unite
    (:pre helm-like-unite/pre
     :post helm-like-unite/post
     :hint nil
     :color amaranth)
"
    Nav    Scroll  Mark             Other            Quit
 ╭─────────────────────────────────────────────────────╮
   ^ ^ _k_ ^ ^     _K_     [_m_] mark         [_v_] view         [_q_] quit
   _h_ ^+^ _l_     ^↕^     [_t_] toggle all   [_d_] delete
   ^ ^ _j_ ^ ^     _J_     [_u_] unmark all   [_f_] follow: %(helm-attr 'follow)
 ╰─────────────────────────────────────────────────────╯
"
  ;; arrows
  ("h" helm-beginning-of-buffer)
  ("j" helm-next-line)
  ("k" helm-previous-line)
  ("l" helm-end-of-buffer)
  ;; beginning/end
  ("g" helm-beginning-of-buffer)
  ("G" helm-end-of-buffer)
  ;; scroll
  ("K" helm-scroll-other-window-down)
  ("J" helm-scroll-other-window)
  ;; mark
  ("t" helm-toggle-all-marks)
  ("m" helm-toggle-visible-mark)
  ("u" helm-unmark-all)
  ;; misc
  ("q" nil)
  ("v" helm-execute-persistent-action)
  ("f" helm-follow-mode)
  ("d" helm-persistent-delete-marked)
  ("/" (lambda ()
          (interactive)
          (execute-kbd-macro [?\C-s]))
       "search")
  ("<escape>" keyboard-escape-quit "quit helm")
  ("?" helm-help "help"))
(define-key helm-map (kbd "<escape>") 'helm-like-unite/body)

(defun helm-persistent-delete-marked ()
  "Kill buffer without quitting helm."
  (interactive)
  (if (equal (cdr (assoc 'name (helm-get-current-source)))
             "Buffers")
      (with-helm-alive-p
        (helm-attrset 'kill-action
                      '(helm-persistent-kill-buffers . never-split))
        (helm-execute-persistent-action 'kill-action))
    (user-error "Only works for buffers")))

(defun helm-persistent-kill-buffers (_buffer)
  (unwind-protect
       (dolist (b (helm-marked-candidates))
         (helm-buffers-persistent-kill-1 b))
    (with-helm-buffer
      (setq helm-marked-candidates nil
            helm-visible-mark-overlays nil))
    (helm-force-update (helm-buffers--quote-truncated-buffer
                        (helm-get-selection)))))



;; -----------------------------------------------------------------------
;; Add keybinding for opening buffers in splits
;;
;; Stolen from: https://github.com/emacs-helm/helm/issues/1100
;; -----------------------------------------------------------------------

(defun helm-buffer-switch-to-new-window-vert (_candidate)
  "Display buffers in new windows."
  ;; Select the bottom right window
  (require 'winner)
  (select-window (car (last (winner-sorted-window-list))))
  ;; Display buffers in new windows
  (dolist (buf (helm-marked-candidates))
    (select-window (split-window-below))
    (switch-to-buffer buf))
  ;; Adjust size of windows
  (balance-windows))

(add-to-list 'helm-type-buffer-actions
             '("Display buffer(s) in new window(s) `M-o'" .
               helm-buffer-switch-new-window) 'append)

(defun helm-buffer-switch-to-new-window (_candidate)
  "Display buffers in new windows."
  ;; Select the bottom right window
  (require 'winner)
  (select-window (car (last (winner-sorted-window-list))))
  ;; Display buffers in new windows
  (dolist (buf (helm-marked-candidates))
    (select-window (split-window-right))
    (switch-to-buffer buf))
  ;; Adjust size of windows
  (balance-windows))

(add-to-list 'helm-type-buffer-actions
             '("Display buffer(s) in new window(s) `M-o'" .
               helm-buffer-switch-new-window) 'append)

(defun helm-buffer-switch-new-window ()
  (interactive)
  (with-helm-alive-p
    (helm-quit-and-execute-action 'helm-buffer-switch-to-new-window-vert)))

(define-key helm-buffer-map (kbd "M-o") #'helm-buffer-switch-new-window)

;; -----------------------------------------------------------------------
