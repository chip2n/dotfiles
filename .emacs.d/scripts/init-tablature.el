(provide 'init-tablature)

(autoload 'chord-mode "tablature-mode" "Guitar tablature." t)
(add-to-list 'auto-mode-alist '("\\.tab\\'" . chord-mode))
(defhydra chip-hydra-tablature
    (:hint nil)
  ("h" tab-backward-char)
  ("j" next-line)
  ("k" previous-line)
  ("l" tab-forward-char))

(defun tab-increment-note () ; ---------------------------------------------------------
"Move current note up one fret"
(interactive)
	(if (tab-check-in-tab)
	(let ((fret (tab-analyze-fret)))
		(if (and (/= fret -1) (<= fret 24)) (progn
		(setq fret (+ fret 1))
		(tab-string (int-to-string fret) tab-current-string)
		))
	)
	; else
	(insert (this-command-keys)))
)

(defun tab-decrement-note () ; ---------------------------------------------------------
"Move current note down one fret"
(interactive)
	(if (tab-check-in-tab)
	(let ((fret (tab-analyze-fret)))
		(if (and (/= fret -1) (>= fret 1)) (progn
		(setq fret (- fret 1))
		(tab-string (int-to-string fret) tab-current-string)
		))
	)
	; else
	(insert (this-command-keys)))
)

(evil-define-key 'normal tab-mode-map
  "h" 'tab-backward-char
  "H" 'tab-backward-barline
  "L" 'tab-forward-barline
  "l" 'tab-forward-char
  "K" 'tab-increment-note
  "J" 'tab-decrement-note
  "x" 'tab-delete-note
  "d" 'tab-delete-chord-forward
  "i" 'tab-insert
  (kbd "RET") 'tab-barline
  "0" (lambda () (interactive) (tab-string "0" tab-current-string))
  "1" (lambda () (interactive) (tab-string "1" tab-current-string))
  "2" (lambda () (interactive) (tab-string "2" tab-current-string))
  "3" (lambda () (interactive) (tab-string "3" tab-current-string))
  "4" (lambda () (interactive) (tab-string "4" tab-current-string))
  "5" (lambda () (interactive) (tab-string "5" tab-current-string))
  "6" (lambda () (interactive) (tab-string "6" tab-current-string))
  "7" (lambda () (interactive) (tab-string "7" tab-current-string))
  "8" (lambda () (interactive) (tab-string "8" tab-current-string))
  "9" (lambda () (interactive) (tab-string "9" tab-current-string))
  )
