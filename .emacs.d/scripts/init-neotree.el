(provide 'init-neotree)

(use-package neotree
  :ensure t
  :config
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
  (setq neo-window-width 40)
  (define-key evil-normal-state-map (kbd "<backspace>") 'neotree-toggle)
  (define-key evil-normal-state-map (kbd "S-<backspace>") 'neotree-project-dir)
  (add-hook 'neotree-mode-hook
	    (lambda ()
              (display-line-numbers-mode -1)
	      (define-key evil-normal-state-local-map (kbd "m") 'chip-hydra-neotree/body)
	      (define-key evil-normal-state-local-map (kbd "o") 'neotree-enter)
	      (define-key evil-normal-state-local-map (kbd "O") 'neotree-quick-look)
	      (define-key evil-normal-state-local-map (kbd "r") 'neotree-refresh)
	      (define-key evil-normal-state-local-map (kbd "s") 'neotree-enter-vertical-split)
	      (define-key evil-normal-state-local-map (kbd "i") 'neotree-enter-horizontal-split)
	      (define-key evil-normal-state-local-map (kbd "I") 'neotree-hidden-file-toggle)
	      (define-key evil-normal-state-local-map (kbd "RET") 'neotree-enter))))

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

;; open neotree at current working directory
(setq neo-smart-open t)

;; hydra for neotree file operations
(defhydra chip-hydra-neotree
    (:color amaranth
     :hint nil)
"
   Nav^            ^Quit
 ╭─^──^───────────^───^─────╮
   [_a_] add     [_q_] quit
   [_c_] copy
   [_m_] move
   [_d_] delete
 ╰─^──^───────────^───^─────╯
"
  ("a" neotree-create-node :exit t)
  ("c" neotree-copy-node :exit t)
  ("m" neotree-rename-node :exit t)
  ("d" neotree-delete-node :exit t)
  ("q" nil))
