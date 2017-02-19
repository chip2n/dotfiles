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
