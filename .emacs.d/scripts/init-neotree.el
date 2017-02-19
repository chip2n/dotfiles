(provide 'init-neotree)

(use-package neotree
  :ensure t)
(setq neo-window-width 40)
(define-key evil-normal-state-map (kbd "<backspace>") 'neotree-toggle)
(define-key evil-normal-state-map (kbd "S-<backspace>") 'neotree-toggle)
(add-hook 'neotree-mode-hook
	  (lambda ()
	    (define-key evil-normal-state-local-map (kbd "o") 'neotree-enter)
	    (define-key evil-normal-state-local-map (kbd "c") 'neotree-create-node)
	    (define-key evil-normal-state-local-map (kbd "d") 'neotree-delete-node)
	    (define-key evil-normal-state-local-map (kbd "r") 'neotree-refresh)
	    (define-key evil-normal-state-local-map (kbd "s") 'neotree-enter-vertical-split)
	    (define-key evil-normal-state-local-map (kbd "i") 'neotree-enter-horizontal-split)
	    (define-key evil-normal-state-local-map (kbd "RET") 'neotree-enter)))

;; stolen from: http://nadeemkhedr.com/emacs-tips-and-best-plugins-to-use-with-evil-mode/
(use-package find-file-in-project
  :ensure t)
(defun neotree-project-dir ()
  "Open NeoTree using the git root."
  (interactive)
  (let ((project-dir (ffip-project-root))
        (file-name (buffer-file-name)))
    (if project-dir
        (progn
        (neotree-dir project-dir)
        (neotree-find file-name))
    (message "Could not find git project root."))))
