;; -*- after-save-hook: c/load-theme; -*-

(deftheme chip)

;;; Groups

(defgroup chip-theme ()
  "Faces defined for my personal theme."
  :group 'chip
  :prefix "chip-theme-"
  :tag "Chip theme")

;;; Colors

(defconst chip-theme-colors-dark
  '((white-1   . "#eceff6")
    (grey-0    . "#1b1e24")
    (grey-1    . "#21242b")
    (grey-2    . "#2c3039")
    (grey-3    . "#494e5a")
    (grey-4    . "#6b7385")
    (grey-5    . "#8790a4")
    (red-1     . "#d98077")
    (red-2     . "#ffa398")
    (green-1   . "#89d2b0")
    (green-2   . "#c5fca4")
    (yellow-1  . "#eae1a6")
    (yellow-2  . "#fbf2bf")
    (blue-1    . "#67acd5")
    (blue-2    . "#a2e7ff")
    (pink-1    . "#dc8cac")
    (pink-2    . "#ffafcd")
    (magenta-1 . "#dc8cac")
    (magenta-2 . "#ecb3c9")

    (color-background        . grey-1)
    (color-foreground        . white-1)
    (color-comment           . grey-4)
    (color-cursor            . grey-4)
    (color-modeline-active   . grey-3)
    (color-modeline-inactive . grey-2)
    (color-highlight-1       . grey-2)
    (color-highlight-2       . grey-3)
    (color-border            . grey-2)
    (color-headline-1        . white-1)
    (color-headline-2        . white-1)
    (color-headline-3        . white-1)
    (color-headline-4        . white-1)
    (color-link              . blue-2)
    (color-link-visited      . blue-1)
    (color-error             . red-2)
    (color-warning           . red-2)
    (color-success           . green-2)
    (color-diff-added        . "#4d785f")
    (color-diff-removed      . "#8c4a4a")
    (color-evil-normal       . grey-3)
    (color-evil-insert       . blue-2)
    (color-evil-visual       . blue-2)
    (color-evil-emacs        . pink-1)
    (color-evil-motion       . pink-1)))

(cl-defmacro define-theme (name &body body)
  `(progn
     (custom-theme-set-faces ',name ,@(mapcar (lambda (s) `',(chip-theme--expand-face-def s)) body))

     (let* (,@(mapcar (lambda (x) `(,(car x) ,(cdr x))) chip-theme-colors-dark))

       (custom-theme-set-variables
        ',name
        `(ansi-color-names-vector [,grey-3 ,red-2 ,green-2 ,yellow-1
                                           ,blue-2 ,magenta-2 ,blue-1 ,grey-4])
        `(rustic-ansi-faces [,grey-3 ,red-2 ,green-2 ,yellow-1
                                     ,blue-2 ,magenta-2 ,blue-1 ,grey-4])))))

(defun chip-theme--lookup-color (color)
  (let ((v (cdr (assoc color chip-theme-colors-dark))))
    (when v
      (if (symbolp v)
          (chip-theme--lookup-color v)
        v))))

(defun chip-theme--resolve-colors (spec)
  (cond
   ((listp spec)
    (let ((result nil))
      (cl-loop while spec
               do (let ((k (pop spec))
                        (v (pop spec)))
                    (push k result)
                    (push (chip-theme--resolve-colors v) result)))
      (nreverse result)))
   ((symbolp spec) (or (chip-theme--lookup-color spec) spec))
   (t spec)))

(defun chip-theme--expand-face-def (def)
  `(,(car def) ((((class color) (min-colors 89))
                 ,(chip-theme--resolve-colors (cadr def))))))

(define-theme chip
  (default (:foreground color-foreground
            :background color-background
            :family "Jetbrains Mono"
            :height 120))
  (fixed-pitch (:family "Jetbrains Mono" :height 120))
  (variable-pitch (:family "Jetbrains Mono" :height 120))
  (cursor (:background color-cursor))
  (parenthesis (:foreground color-comment))

  ;; Highlighting faces
  (fringe (:foreground color-comment :background color-background))
  (highlight (:background color-highlight-1))
  (region (:background color-highlight-2 :extend t))
  (secondary-selection (:background color-highlight-1 :extend t))
  (isearch (:foreground white-1 :background blue-1))
  (lazy-highlight (:background color-highlight-2))
  (trailing-whitespace (:background color-error))
  (vertical-border (:foreground color-border))

  (window-divider (:foreground color-border))

  (link (:foreground blue-2 :underline t))
  (link-visited (:foreground blue-1 :underline t))
  (line-number (:inherit default :foreground grey-3))
  (line-number-current-line (:inherit default
                             :foreground color-foreground
                             :background color-highlight-2
                             :bold t))
  (help-key-binding (:foreground pink-2 :underline t :bold t))
  (bookmark-face (:background color-background :foreground color-comment))

  ;; Paren match
  (show-paren-match (:background red-2 :foreground color-background))

  ;; Header faces
  (header-line (:box (:line-width 4 :color color-background)
                :background color-background
                :foreground color-foreground))

  ;; Mode line faces
  (mode-line (:background color-modeline-active :foreground color-foreground))
  (mode-line-inactive (:background color-modeline-inactive :foreground color-comment))
  (mode-line-highlight (:background nil :foreground color-foreground))
  (mode-line-evil-state-normal (:foreground color-evil-normal))
  (mode-line-evil-state-insert (:foreground color-evil-insert))
  (mode-line-evil-state-visual (:foreground color-evil-visual))
  (mode-line-evil-state-motion (:foreground color-evil-motion))
  (mode-line-evil-state-emacs (:foreground color-evil-emacs))

  ;; Info
  (Info-quoted (:foreground pink-2 :bold t))
  (info-menu-star (:inherit default))

  ;; Telephone line faces
  (telephone-line-evil-normal (:background color-evil-normal :foreground color-foreground :weight bold))
  (telephone-line-evil-insert (:background color-evil-insert :foreground color-foreground :weight bold))
  (telephone-line-evil-visual (:background color-evil-visual :foreground color-foreground :weight bold))
  (telephone-line-evil-emacs (:background color-evil-emacs :foreground color-foreground :weight bold))
  (telephone-line-evil-motion (:background color-evil-motion :foreground color-foreground :weight bold))
  (telephone-line-accent-active (:background color-modeline-active :foreground color-foreground))
  (telephone-line-accent-inactive (:background color-modeline-inactive :foreground grey-3))

  ;; Pulse (e.g. when jumping with xref)
  (pulse-highlight-start-face (:background red-1))

  ;; Term colors
  (term-color-black (:foreground grey-3))
  (term-color-red (:foreground red-2))
  (term-color-blue (:foreground blue-2))
  (term-color-green (:foreground green-2))
  (term-color-yellow (:foreground yellow-2))
  (term-color-pink (:foreground pink-2))
  (term-color-magenta (:foreground magenta-2))
  (vterm-color-black (:foreground grey-3))
  (vterm-color-red (:foreground red-2))
  (vterm-color-blue (:foreground blue-2))
  (vterm-color-green (:foreground green-2))
  (vterm-color-yellow (:foreground yellow-2))
  (vterm-color-pink (:foreground pink-2))
  (vterm-color-magenta (:foreground magenta-2))

  ;; ANSI colors
  (ansi-color-red (:foreground red-1))
  (ansi-color-bright-red (:foreground red-2))
  (ansi-color-blue (:foreground blue-1))
  (ansi-color-bright-blue (:foreground blue-2))
  (ansi-color-green (:foreground green-2))
  (ansi-color-bright-green (:foreground green-2))
  (ansi-color-yellow (:foreground yellow-2))
  (ansi-color-bright-yellow (:foreground yellow-2))

  ;; compilation-mode
  (compilation-mode-line-exit (:foreground green-2))
  (compilation-mode-line-run (:foreground red-2))
  (compilation-mode-line-fail (:foreground red-2))

  ;; Ivy
  (ivy-virtual (:foreground white-1))

  ;; selectrum
  (selectrum-primary-highlight (:foreground color-foreground :background color-highlight-2))
  (selectrum-current-candidate (:foreground red-2 :background color-highlight-1))
  (selectrum-group-title (:foreground color-comment))
  (selectrum-group-separator (:foreground color-comment :strike-through t))

  ;; Vertico
  (vertico-current (:foreground red-2 :background color-highlight-1))
  (vertico-group-title (:foreground color-comment))
  (vertico-group-separator (:foreground color-highlight-2 :strike-through t))
  (vertico-posframe (:background grey-0))
  (vertico-posframe-border (:background grey-0))

  ;; Marginalia
  (marginalia-documentation (:foreground color-comment))

  ;; Posframe
  (ivy-posframe (:background grey-2 :foreground white-1))
  (ivy-posframe-border (:background grey-2))

  ;;  Eshell
  (eshell-ls-executable (:foreground green-2 :weight bold))

  ;; Company
  (company-tooltip (:background grey-2 :foreground white-1))
  (company-tooltip-selection (:background grey-3 :foreground white-1))
  (company-tooltip-annotation (:foreground grey-4 :slant italic))
  (company-tooltip-annotation-selection (:foreground white-1 :slant italic))
  (company-tooltip-common (:foreground green-2 :weight bold))
  (company-scrollbar-bg (:background grey-3))
  (company-scrollbar-fg (:background green-2))

  ;; LSP
  (lsp-lsp-flycheck-warning-unnecessary-face (:underline (:color color-warning :style wave)))
  (lsp-lens-face (:foreground color-comment :slant italic))
  (lsp-lens-mouse-face (:foreground color-link :underline t :slant italic))

  ;; Flycheck
  (flycheck-info (:underline (:color green-1 :style wave)))
  (flycheck-warning (:underline (:color yellow-1 :style wave)))
  (flycheck-error (:underline (:color red-1 :style wave)))

  ;; Avy
  (avy-lead-face (:background red-1 :foreground grey-2 :weight bold))
  (avy-lead-face-0 (:background red-1 :foreground grey-2 :weight bold))

  ;; ace-window
  (aw-background-face (:foreground grey-3))
  (aw-leading-char-face (:foreground color-foreground))

  ;; Swiper
  (swiper-line-face (:background color-highlight-1 :extend t))
  (swiper-match-face-1 (:background white-1 :foreground grey-2 :weight bold))
  (swiper-background-match-face-1 (:background color-highlight-2
                                   :foreground white-1
                                   :weight bold))

  ;; Hydra
  (hydra-face-red (:foreground red-1))
  (hydra-face-amaranth (:foreground pink-1))
  (hydra-face-teal (:foreground blue-1))

  ;; Org
  (org-default (:foreground white-1 :underline nil))
  (org-ellipsis (:foreground grey-4))
  (org-todo (:foreground red-2 :bold t :underline nil))
  (org-done (:foreground green-2 :bold t :underline nil))
  (org-headline-todo (:foreground color-foreground :bold t))
  (org-headline-done (:foreground color-foreground :bold t))
  (org-headline-content (:inherit org-default :bold t :underline t))
  (org-level-1 (:foreground color-headline-1 :bold t))
  (org-level-2 (:foreground color-headline-2 :bold t))
  (org-level-3 (:foreground color-headline-3 :bold t))
  (org-level-4 (:foreground color-headline-4 :bold t))
  (org-level-5 (:foreground color-headline-1 :bold t))
  (org-level-6 (:foreground color-headline-2 :bold t))
  (org-level-7 (:foreground color-headline-3 :bold t))
  (org-level-8 (:foreground color-headline-4 :bold t))
  (org-link (:foreground color-link :underline t))
  (org-date (:foreground color-comment))
  (org-tag (:foreground color-comment :slant italic :underline nil))
  (org-drawer (:foreground color-comment :slant italic))
  (org-property-value (:foreground color-comment :slant italic))
  (org-document-info (:foreground color-comment :slant italic))
  (org-document-info-keyword (:foreground color-comment :slant italic))
  (org-document-title (:inherit default :underline t :weight bold))
  (org-special-keyword (:foreground color-comment :slant italic))
  (org-upcoming-deadline (:foreground white-1))
  (org-upcoming-distant-deadline (:foreground grey-4))
  (org-warning (:foreground color-error))
  (org-code (:foreground pink-2 :weight bold :inherit fixed-pitch))
  (org-verbatim (:foreground pink-2 :weight bold :inherit fixed-pitch))
  (org-block-begin-line (:background "#383d4a" :foreground grey-5 :extend t))
  (org-block (:background grey-2))
  (org-block-end-line (:background "#383d4a" :foreground grey-5 :extend t))
  (org-mode-line-clock (:foreground nil))

  ;; Org bullets
  (org-bullet (:foreground red-2 :bold t :underline nil))
  (org-superstar-leading (:foreground red-2 :bold t))
  (org-superstar-header-bullet (:foreground red-2 :bold t))

  ;; Org-agenda
  (org-agenda-structure (:foreground grey-4 :weight bold))
  (org-agenda-date (:foreground color-comment :weight bold))
  (org-agenda-date-today (:foreground color-comment :weight bold))
  (org-agenda-date-weekend (:foreground color-comment :weight bold))
  (org-time-grid (:foreground color-comment))
  (org-agenda-current-time (:foreground pink-2 :weight bold))
  (org-super-agenda-header (:foreground grey-4 :weight bold))
  (org-scheduled-previously (:foreground red-2))
  (org-scheduled (:foreground color-foreground))
  (org-scheduled-today (:foreground color-foreground))
  (org-deadline (:foreground color-foreground))
  (org-agenda-dimmed-todo-face (:foreground color-comment))

  ;; outshine
  (outshine-level-1 (:foreground color-headline-1 :bold t :underline (:color grey-3) :extend t))
  (outshine-level-2 (:foreground color-headline-2 :bold t :underline (:color grey-3) :extend t))
  (outshine-level-3 (:foreground color-headline-3 :bold t :underline (:color grey-3) :extend t))
  (outshine-level-4 (:foreground color-headline-4 :bold t :underline (:color grey-3) :extend t))
  (outshine-level-5 (:foreground color-headline-1 :bold t :underline (:color grey-3) :extend t))
  (outshine-level-6 (:foreground color-headline-2 :bold t :underline (:color grey-3) :extend t))
  (outshine-level-7 (:foreground color-headline-3 :bold t :underline (:color grey-3) :extend t))
  (outshine-level-8 (:foreground color-headline-4 :bold t :underline (:color grey-3) :extend t))

  ;; Magit faces
  (magit-section-highlight (:background color-highlight-1))
  (magit-section-heading (:foreground pink-2 :weight bold))
  (magit-hash (:foreground grey-4))
  (magit-dimmed (:foreground grey-4))
  (magit-signature-error (:foreground red-1))
  (magit-log-author (:foreground red-1))
  (magit-log-date (:foreground grey-4))
  (magit-log-graph (:foreground grey-4))
  (magit-diff-added (:background "#3e4d4b" :foreground white-1))
  (magit-diff-added-highlight (:background color-diff-added :foreground white-1))
  (magit-diff-removed (:background "#3d373d" :foreground white-1))
  (magit-diff-removed-highlight (:background color-diff-removed :foreground white-1))
  (magit-diff-hunk-heading (:background grey-2 :foreground grey-4))
  (magit-diff-hunk-heading-highlight (:background grey-3 :foreground white-1))
  (magit-diff-context (:foreground grey-3))
  (magit-diff-context-highlight (:background grey-2 :foreground white-1))
  (magit-head (:foreground blue-2 :weight bold))
  (magit-branch-local (:foreground blue-2 :weight bold))
  (magit-branch-remote (:foreground green-2 :weight bold))
  (magit-branch-remote-head (:box (:line-width 1) :foreground green-2))
  (magit-branch-current (:box (:line-width 1) :foreground blue-2))
  (magit-tag (:foreground yellow-1 :weight bold))
  (magit-header-line nil)
  (magit-blame-highlight (:foreground color-comment :background grey-2))
  (git-commit-summary (:foreground white-1))

  ;; Pulsar
  ;; (pulsar-generic (:background grey-3))

  ;; git-gutter
  (git-gutter:added (:foreground green-2 :bold t))
  (git-gutter:deleted (:foreground red-1 :bold t))
  (git-gutter:modified (:foreground color-comment :bold t))

  ;; debugging
  (breakpoint-enabled (:foreground red-1))
  (breakpoint-disabled (:foreground color-comment))
  (realgud-bp-line-enabled-face (:background color-diff-removed :extend t))
  (realgud-overlay-arrow1 (:foreground red-1))
  (realgud-bp-enabled-face (:foreground color-background))

  ;; Elfeed faces
  (message-header-name (:foreground grey-3 :weight bold))
  (message-header-subject (:foreground blue-2 :weight bold))
  (message-header-to (:foreground grey-4 :weight bold))
  (message-header-other (:foreground pink-2))
  (message-header-other (:foreground pink-2))
  (elfeed-search-date-face (:foreground color-headline-1 :weight bold))
  (elfeed-search-tag-face (:foreground color-comment))
  (elfeed-search-feed-face (:foreground color-headline-2 :weight bold))
  (elfeed-search-unread-title-face (:foreground color-foreground))
  (elfeed-search-title-face (:foreground color-comment))

  ;; Neotree
  (neo-dir-link-face (:foreground color-link :weight bold))
  (neo-root-dir-face (:foreground color-headline-1 :weight bold))

  ;; Treemacs
  (treemacs-root-face (:foreground color-foreground :weight bold))
  (treemacs-file-face (:foreground color-foreground))
  (treemacs-git-ignored-face (:foreground color-foreground))
  (treemacs-git-modified-face (:foreground color-foreground))
  (treemacs-git-added-face (:foreground color-foreground))
  (treemacs-git-untracked-face (:foreground color-foreground))
  (treemacs-directory-face (:foreground color-foreground))
  (treemacs-pulse-on-success (:background color-success))
  (treemacs-pulse-on-error (:background color-error))

  (chip-theme-treemacs-root-face (:foreground color-foreground))
  (chip-theme-treemacs-file-face (:foreground color-foreground))

  ;; Calfw
  (cfw:face-title (:foreground color-foreground))
  (cfw:face-toolbar (:background color-background))
  (cfw:face-toolbar-button-off (:background color-background :foreground color-comment))
  (cfw:face-toolbar-button-on (:background color-background :foreground blue-2))
  (cfw:face-grid (:foreground color-highlight-1))
  (cfw:face-select (:background color-highlight-1))
  (cfw:face-day-title (:background color-background))
  (cfw:face-today-title (:background color-highlight-1))
  (cfw:face-holiday (:foreground color-foreground))
  (cfw:face-sunday (:foreground color-foreground))
  (cfw:face-disable (:foreground color-comment))
  (cfw:face-header (:foreground color-foreground))

  ;; Lispy
  (lispy-face-hint (:foreground grey-4 :weight bold))

  ;; CIDER
  (cider-test-success-face (:foreground color-success :weight bold))
  (cider-test-failure-face (:background color-error   :foreground white-1 :weight bold))
  (cider-fringe-good-face (:foreground color-comment))

  ;; comint
  (comint-highlight-input (:inherit default))

  ;; SLIME faces
  (slime-repl-inputed-output-face (:foreground color-comment))

  ;; Sly faces
  (sly-mrepl-output-face (:foreground color-comment))
  (sly-mrepl-note-face (:foreground color-comment))
  (sly-mrepl-prompt-face (:foreground color-foreground :weight bold))
  (sly-part-button-face (:foreground blue-2 :weight bold))

  ;; Macrostep faces
  (macrostep-expansion-highlight-face (:background color-highlight-1 :extend t))

  ;; Slack faces
  (slack-message-output-header (:foreground blue-2 :weight bold))
  (slack-message-output-reaction (:foreground grey-4))
  (lui-button-face (:foreground blue-2 :underline t))
  (lui-time-stamp-face (:foreground pink-2 :weight bold))

  ;; Erc faces
  (erc-nick-default-face (:foreground green-1 :weight bold))
  (erc-current-nick-face (:foreground green-1 :weight bold))
  (erc-notice-face (:foreground grey-3))
  (erc-input-face (:foreground white-1))
  (erc-timestamp-face (:foreground grey-3 :weight bold))
  (erc-prompt-face (:foreground grey-3 :weight bold))
  (erc-nick-msg-face (:foreground red-1))
  (erc-direct-msg-face (:foreground red-2))

  ;; EIN
  (ein:cell-input-area (:background grey-2))
  (ein:cell-output-area (:foreground grey-4))

  ;; notmuch
  (notmuch-search-unread-face (:weight bold))
  (notmuch-message-summary-face (:background grey-2))
  (notmuch-tag-unread (:foreground red-2))
  (notmuch-tag-face (:foreground green-2))
  (notmuch-show-part-button-default (:inherit link))
  (message-mml (:inherit link))
  (widget-field (:background grey-3))
  (widget-button (:inherit link))
  (widget-button-pressed (:background grey-2 :foreground color-foreground))

  ;; Pomidor
  (pomidor-time-face (:foreground green-1 :weight bold))
  (pomidor-timer-face (:foreground green-1 :weight bold))
  (pomidor-break-face (:foreground blue-2 :weight bold))
  (pomidor-work-face (:foreground green-1 :weight bold))
  (pomidor-overwork-face (:foreground red-2 :weight bold))

  ;; Escape and prompt faces
  (minibuffer-prompt (:weight bold :foreground grey-3))
  (escape-glyph (:foreground red-2))
  (homoglyph (:foreground red-2))
  (error (:foreground color-error))
  (warning (:foreground color-error))
  (success (:foreground color-success))

  ;; Font lock faces
  (font-lock-builtin-face (:foreground pink-2))
  (font-lock-comment-face (:foreground color-comment))
  (font-lock-constant-face (:foreground blue-2))
  (font-lock-function-name-face (:foreground pink-2))
  (font-lock-keyword-face (:foreground blue-2))
  (font-lock-string-face (:foreground green-2))
  (font-lock-type-face (:foreground red-2))
  (font-lock-variable-name-face (:foreground pink-2))

  ;; Markdown faces
  (markdown-markup-face (:foreground grey-3))

  ;; Message faces
  (message-header-name (:foreground blue-2))
  (message-header-cc (:foreground yellow-1))
  (message-header-other (:foreground red-2))
  (message-header-subject (:foreground red-2))
  (message-header-to (:weight bold :foreground yellow-1))
  (message-cited-text (:slant italic :foreground grey-4))
  (message-separator (:weight bold :foreground green-2))

  ;; Tabbar faces
  (tabbar-default
   (:background red-1 :foreground blue-1)))

(provide-theme 'chip)
