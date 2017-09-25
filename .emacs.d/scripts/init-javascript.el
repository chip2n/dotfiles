(provide 'init-javascript)

(setq js-indent-level 2)

;; ignore shebangs
(setq js2-skip-preprocessor-directives t)

(use-package js2-mode
  :ensure t)
(add-to-list 'auto-mode-alist (cons (rx ".js" eos) 'js2-mode))

;; disable semicolon warnings
(setq js2-strict-missing-semi-warning nil)
(setq js2-missing-semi-one-line-override t)

;; disable inconsistent return warnings
(setq js2-strict-inconsistent-return-warning nil)

(use-package json-mode
  :ensure t)

(use-package js2-refactor
  :ensure t)
(use-package xref-js2
  :ensure t)
(add-hook 'js2-mode-hook #'js2-refactor-mode)
(js2r-add-keybindings-with-prefix "C-c C-r")
(define-key js2-mode-map (kbd "C-k") #'js2r-kill)
;; js-mode (which js2 is based on) binds "M-." which conflicts with xref, so
;; unbind it.
(define-key js-mode-map (kbd "M-.") nil)

(add-hook 'js2-mode-hook (lambda ()
  (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)))

;; =============================================================
;; Multiple cursors evil compat (use emacs mode during mc)
;; =============================================================
(defvar mc-evil-compat/evil-prev-state nil)
(defvar mc-evil-compat/mark-was-active nil)

(defun user-utils/evil-visual-or-normal-p ()
  "True if evil mode is enabled, and we are in normal or visual mode."
  (and (bound-and-true-p evil-mode)
       (not (memq evil-state '(insert emacs)))))

(defun mc-evil-compat/switch-to-emacs-state ()
  (when (user-utils/evil-visual-or-normal-p)

    (setq mc-evil-compat/evil-prev-state evil-state)

    (when (region-active-p)
      (setq mc-evil-compat/mark-was-active t))

    (let ((mark-before (mark))
          (point-before (point)))

      (evil-emacs-state 1)

      (when (or mc-evil-compat/mark-was-active (region-active-p))
        (goto-char point-before)
        (set-mark mark-before)))))

(defun mc-evil-compat/back-to-previous-state ()
  (when mc-evil-compat/evil-prev-state
    (unwind-protect
        (case mc-evil-compat/evil-prev-state
          ((normal visual) (evil-force-normal-state))
          (t (message "Don't know how to handle previous state: %S"
                      mc-evil-compat/evil-prev-state)))
      (setq mc-evil-compat/evil-prev-state nil)
      (setq mc-evil-compat/mark-was-active nil))))

(add-hook 'multiple-cursors-mode-enabled-hook
          'mc-evil-compat/switch-to-emacs-state)
(add-hook 'multiple-cursors-mode-disabled-hook
          'mc-evil-compat/back-to-previous-state)

(defun mc-evil-compat/rectangular-switch-state ()
  (if rectangular-region-mode
      (mc-evil-compat/switch-to-emacs-state)
    (setq mc-evil-compat/evil-prev-state nil)))

;; When running edit-lines, point will return (position + 1) as a
;; result of how evil deals with regions
(defadvice mc/edit-lines (before change-point-by-1 activate)
  (when (user-utils/evil-visual-or-normal-p)
    (if (> (point) (mark))
        (goto-char (1- (point)))
      (push-mark (1- (mark))))))

(add-hook 'rectangular-region-mode-hook 'mc-evil-compat/rectangular-switch-state)

(defvar mc--default-cmds-to-run-once nil)
