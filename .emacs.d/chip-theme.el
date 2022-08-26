(deftheme chip
  "Face colors using the chip palette.")

(defvar colors-dark
  '((white-1   . "#eceff6")
    (grey-0    . "#1b1e24")
    (grey-1    . "#21242b")
    (grey-2    . "#2c3039")
    (grey-3    . "#494e5a")
    (grey-4    . "#6b7385")
    (grey-5    . "#b6bed0")
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
    (magenta-2 . "#ecb3c9")))

(defvar theme-dark
  '((color-background        . grey-1)
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

(defvar colors-light
  '((white-1   . "#F9F5ED")
    (black-1   . "#19150f")
    (grey-1    . "#F2E2C9")
    (grey-2    . "#DECAAA")
    (grey-3    . "#C2AF90")
    (grey-4    . "#9b896c")
    (red-1     . "#CB6161")
    (red-2     . "#ca4e4e")
    (green-1   . "#7AA949")
    (green-2   . "#64af1a")
    (yellow-1  . "#eae1a6")
    (yellow-2  . "#fbf2bf")
    (blue-1    . "#529DC7")
    (blue-2    . "#1191d9")
    (pink-1    . "#D17CD2")
    (pink-2    . "#bd42be")
    (magenta-1 . "#dc8cac")
    (magenta-2 . "#ecb3c9")))

(defvar theme-light
  '((color-background        . white-1)
    (color-foreground        . black-1)
    (color-comment           . grey-4)
    (color-cursor            . grey-3)
    (color-modeline-active   . grey-1)
    (color-modeline-inactive . grey-1)
    (color-highlight-1       . grey-1)
    (color-highlight-2       . grey-2)
    (color-border            . "#7B6B52")
    (color-headline-1        . black-1)
    (color-headline-2        . black-1)
    (color-headline-3        . black-1)
    (color-headline-4        . black-1)
    (color-link              . blue-2)
    (color-link-visited      . blue-1)
    (color-error             . red-2)
    (color-warning           . red-2)
    (color-success           . green-2)
    (color-diff-added        . "#4d785f")
    (color-diff-removed      . "#8c4a4a")
    (color-evil-normal       . grey-2)
    (color-evil-insert       . blue-2)
    (color-evil-visual       . blue-2)
    (color-evil-emacs        . pink-1)
    (color-evil-motion       . pink-1)))

(defmacro with-theme (name &rest body)
  (declare (indent 1))
  `(let* ((class '((class color) (min-colors 89)))
          ,@(mapcar (lambda (x) `(,(car x) ,(cdr x))) colors-dark)
          ,@(mapcar (lambda (x) `(,(car x) ,(cdr x))) theme-dark))
     (custom-theme-set-faces ,name ,@body)

     (custom-theme-set-variables
      ,name
      `(ansi-color-names-vector [,grey-3 ,red-2 ,green-2 ,yellow-1
                                         ,blue-2 ,magenta-2 ,blue-1 ,grey-4])
      `(rustic-ansi-faces [,grey-3 ,red-2 ,green-2 ,yellow-1
                                   ,blue-2 ,magenta-2 ,blue-1 ,grey-4]))))

(with-theme 'chip
  `(default ((,class (:foreground ,color-foreground
                      :background ,color-background
                      :family "Jetbrains Mono"
                      :height 100
                      ))))
  `(fixed-pitch ((,class (:family "Jetbrains Mono" :height 100))))
  `(variable-pitch ((,class (:family "Jetbrains Mono" :height 100))))
  `(cursor ((,class (:background ,color-cursor))))
  `(parenthesis ((,class (:foreground ,color-comment))))

  ;; Highlighting faces
  `(fringe ((,class (:foreground ,color-comment :background ,color-background))))
  `(highlight ((,class (:background ,color-highlight-1))))
  `(region ((,class (:background ,color-highlight-2 :extend t))))
  `(secondary-selection ((,class (:background ,color-highlight-1 :extend t))))
  `(isearch ((,class (:foreground ,white-1 :background ,blue-1))))
  `(lazy-highlight ((,class (:background ,color-highlight-2))))
  `(trailing-whitespace ((,class (:background ,color-error))))
  `(vertical-border ((,class (:foreground ,color-border))))

  `(window-divider ((,class (:foreground ,color-border))))

  `(link ((,class (:foreground ,blue-2 :underline t))))
  `(link-visited ((,class (:foreground ,blue-1 :underline t))))
  `(line-number ((,class (:inherit default :foreground ,grey-3))))
  `(line-number-current-line ((,class (:inherit default
                                       :foreground ,color-foreground
                                       :background ,color-highlight-1
                                       :bold t))))
  `(help-key-binding ((,class (:foreground ,pink-2 :underline t :bold t))))
  `(bookmark-face ((,class ())))

  ;; Paren match
  `(show-paren-match ((,class (:background ,color-highlight-2))))

  ;; Header faces
  ;; `(header-line ((,class (:background ,color-modeline-active
  ;;                         :foreground ,color-foreground))))
  `(header-line ((,class (:box (:line-width 4 :color ,color-background)
                          :background ,color-background
                          :foreground ,color-foreground))))

  ;; Mode line faces
  `(mode-line ((,class (:background ,color-modeline-active :foreground ,color-foreground))))
  `(mode-line-inactive ((,class (:background ,color-modeline-inactive :foreground ,color-comment))))
  `(mode-line-highlight ((,class (:background nil :foreground ,color-foreground))))
  `(mode-line-evil-state-normal ((,class (:foreground ,color-evil-normal))))
  `(mode-line-evil-state-insert ((,class (:foreground ,color-evil-insert))))
  `(mode-line-evil-state-visual ((,class (:foreground ,color-evil-visual))))
  `(mode-line-evil-state-motion ((,class (:foreground ,color-evil-motion))))
  `(mode-line-evil-state-emacs ((,class (:foreground ,color-evil-emacs))))

  ;; Info
  `(Info-quoted ((,class (:foreground ,pink-2 :bold t))))
  `(info-menu-star ((,class (:inherit default))))

  ;; Telephone line faces
  `(telephone-line-evil-normal
    ((,class (:background ,color-evil-normal :foreground ,color-foreground :weight bold))))
  `(telephone-line-evil-insert
    ((,class (:background ,color-evil-insert :foreground ,color-foreground :weight bold))))
  `(telephone-line-evil-visual
    ((,class (:background ,color-evil-visual :foreground ,color-foreground :weight bold))))
  `(telephone-line-evil-emacs
    ((,class (:background ,color-evil-emacs :foreground ,color-foreground :weight bold))))
  `(telephone-line-evil-motion
    ((,class (:background ,color-evil-motion :foreground ,color-foreground :weight bold))))
  `(telephone-line-accent-active
    ((,class (:background ,color-modeline-active :foreground ,color-foreground))))
  `(telephone-line-accent-inactive
    ((,class (:background ,color-modeline-inactive :foreground ,grey-3))))

  ;; Pulse (e.g. when jumping with xref)
  `(pulse-highlight-start-face ((,class (:background ,red-1))))

  ;; Term colors
  `(term-color-black ((,class (:foreground ,grey-3))))
  `(term-color-red ((,class (:foreground ,red-2))))
  `(term-color-blue ((,class (:foreground ,blue-2))))
  `(term-color-green ((,class (:foreground ,green-2))))
  `(term-color-yellow ((,class (:foreground ,yellow-2))))
  `(term-color-pink ((,class (:foreground ,pink-2))))
  `(term-color-magenta ((,class (:foreground ,magenta-2))))
  `(term-color-magenta ((,class (:foreground ,magenta-2))))
  `(vterm-color-black ((,class (:foreground ,grey-3))))
  `(vterm-color-red ((,class (:foreground ,red-2))))
  `(vterm-color-blue ((,class (:foreground ,blue-2))))
  `(vterm-color-green ((,class (:foreground ,green-2))))
  `(vterm-color-yellow ((,class (:foreground ,yellow-2))))
  `(vterm-color-pink ((,class (:foreground ,pink-2))))
  `(vterm-color-magenta ((,class (:foreground ,magenta-2))))
  `(vterm-color-magenta ((,class (:foreground ,magenta-2))))

  ;; compilation-mode
  `(compilation-mode-line-exit ((,class (:foreground ,green-2))))
  `(compilation-mode-line-run ((,class (:foreground ,red-2))))
  `(compilation-mode-line-fail ((,class (:foreground ,red-2))))

  ;; Ivy
  `(ivy-virtual ((,class (:foreground ,white-1))))

  ;; selectrum
  `(selectrum-primary-highlight ((,class (:foreground ,color-foreground :background ,color-highlight-2))))
  `(selectrum-current-candidate ((,class (:foreground ,red-2 :background ,color-highlight-1))))
  `(selectrum-group-title ((,class (:foreground ,color-comment))))
  `(selectrum-group-separator ((,class (:foreground ,color-comment :strike-through t))))

  ;; Vertico
  `(vertico-current ((,class (:foreground ,red-2 :background ,color-highlight-1))))
  `(vertico-group-title ((,class (:foreground ,color-comment))))
  `(vertico-group-separator ((,class (:foreground ,color-highlight-2 :strike-through t))))

  ;; Marginalia
  `(marginalia-documentation ((,class (:foreground ,color-comment))))

  ;; Posframe
  `(ivy-posframe ((,class (:background ,grey-2 :foreground ,white-1))))
  `(ivy-posframe-border ((,class (:background ,grey-2))))

  ;;  Eshell
  `(eshell-ls-executable ((,class (:foreground ,green-2 :weight bold))))

  ;; Company
  `(company-tooltip ((,class (:background ,grey-2 :foreground ,white-1))))
  `(company-tooltip-selection ((,class (:background ,grey-3 :foreground ,white-1))))
  `(company-tooltip-annotation ((,class (:foreground ,grey-4 :slant italic))))
  `(company-tooltip-annotation-selection ((,class (:foreground ,white-1 :slant italic))))
  `(company-tooltip-common ((,class (:foreground ,green-2 :weight bold))))
  `(company-scrollbar-bg ((,class (:background ,grey-3))))
  `(company-scrollbar-fg ((,class (:background ,green-2))))

  ;; LSP
  `(lsp-lsp-flycheck-warning-unnecessary-face ((,class (:underline (:color ,color-warning :style wave)))))
  `(lsp-lens-face ((,class (:foreground ,color-comment :slant italic))))
  `(lsp-lens-mouse-face ((,class (:foreground ,color-link :underline t :slant italic))))

  ;; Avy
  `(avy-lead-face ((,class (:background ,red-1 :foreground ,grey-2 :weight bold))))
  `(avy-lead-face-0 ((,class (:background ,red-1 :foreground ,grey-2 :weight bold))))

  ;; ace-window
  `(aw-background-face ((,class (:foreground ,grey-3))))
  `(aw-leading-char-face ((,class (:foreground ,color-foreground))))

  ;; Swiper
  `(swiper-line-face ((,class (:background ,color-highlight-1 :extend t))))
  `(swiper-match-face-1 ((,class (:background ,white-1 :foreground ,grey-2 :weight bold))))
  `(swiper-background-match-face-1 ((,class (:background ,color-highlight-2
                                             :foreground ,white-1
                                             :weight bold))))

  ;; Hydra
  `(hydra-face-red ((,class (:foreground ,red-1))))
  `(hydra-face-amaranth ((,class (:foreground ,pink-1))))
  `(hydra-face-teal ((,class (:foreground ,blue-1))))

  ;; Org
  `(org-default ((,class (:foreground ,white-1 :underline nil))))
  `(org-ellipsis ((,class (:foreground ,grey-4))))
  `(org-todo ((,class (:foreground ,red-2 :bold t :underline nil))))
  `(org-done ((,class (:foreground ,green-2 :bold t :underline nil))))
  `(org-headline-todo ((,class (:foreground ,color-foreground :bold t))))
  `(org-headline-done ((,class (:foreground ,color-foreground :bold t))))
  `(org-headline-content ((,class (:inherit org-default :bold t :underline t))))
  `(org-level-1 ((,class (:foreground ,color-headline-1 :bold t))))
  `(org-level-2 ((,class (:foreground ,color-headline-2 :bold t))))
  `(org-level-3 ((,class (:foreground ,color-headline-3 :bold t))))
  `(org-level-4 ((,class (:foreground ,color-headline-4 :bold t))))
  `(org-level-5 ((,class (:foreground ,color-headline-1 :bold t))))
  `(org-level-6 ((,class (:foreground ,color-headline-2 :bold t))))
  `(org-level-7 ((,class (:foreground ,color-headline-3 :bold t))))
  `(org-level-8 ((,class (:foreground ,color-headline-4 :bold t))))
  `(org-link ((,class (:foreground ,color-link :underline t))))
  `(org-date ((,class (:foreground ,color-comment))))
  `(org-tag ((,class (:foreground ,color-comment :slant italic :underline nil))))
  `(org-drawer ((,class (:foreground ,color-comment :slant italic))))
  `(org-property-value ((,class (:foreground ,color-comment :slant italic))))
  `(org-document-info ((,class (:foreground ,color-comment :slant italic))))
  `(org-document-info-keyword ((,class (:foreground ,color-comment :slant italic))))
  `(org-document-title ((,class (:foreground ,color-comment))))
  `(org-special-keyword ((,class (:foreground ,color-comment :slant italic))))
  `(org-upcoming-deadline ((,class (:foreground ,white-1))))
  `(org-upcoming-distant-deadline ((,class (:foreground ,grey-4))))
  `(org-warning ((,class (:foreground ,color-error))))
  `(org-code ((,class (:foreground ,pink-2 :weight bold :inherit fixed-pitch))))
  `(org-verbatim ((,class (:foreground ,pink-2 :weight bold :inherit fixed-pitch))))
  `(org-block-begin-line ((,class (:foreground ,color-comment :background ,grey-2 :extend t))))
  `(org-block ((,class (:background ,grey-2))))
  `(org-block-end-line ((,class (:foreground ,color-comment :background ,grey-2 :extend t))))
  `(org-mode-line-clock ((,class (:foreground nil))))

  ;; Org bullets
  `(org-bullet ((,class (:foreground ,red-2 :bold t :underline nil))))
  `(org-superstar-leading ((,class (:foreground ,red-2 :bold t))))
  `(org-superstar-header-bullet ((,class (:foreground ,red-2 :bold t))))

  ;; Org-agenda
  `(org-agenda-structure ((,class (:foreground ,grey-4 :weight bold))))
  `(org-agenda-date ((,class (:foreground ,color-comment :weight bold))))
  `(org-agenda-date-today ((,class (:foreground ,color-comment :weight bold))))
  `(org-agenda-date-weekend ((,class (:foreground ,color-comment :weight bold))))
  `(org-time-grid ((,class (:foreground ,color-comment))))
  `(org-agenda-current-time ((,class (:foreground ,pink-2 :weight bold))))
  `(org-super-agenda-header ((,class (:foreground ,grey-4 :weight bold))))
  `(org-scheduled-previously ((,class (:foreground ,red-2))))
  `(org-scheduled ((,class (:foreground ,color-foreground))))
  `(org-scheduled-today ((,class (:foreground ,color-foreground))))
  `(org-deadline ((,class (:foreground ,color-foreground))))
  `(org-agenda-dimmed-todo-face ((,class (:foreground ,color-comment))))

  ;; outshine
  `(outshine-level-1 ((,class (:foreground ,color-headline-1 :bold t :underline (:color ,grey-3) :extend t))))
  `(outshine-level-2 ((,class (:foreground ,color-headline-2 :bold t :underline (:color ,grey-3) :extend t))))
  `(outshine-level-3 ((,class (:foreground ,color-headline-3 :bold t :underline (:color ,grey-3) :extend t))))
  `(outshine-level-4 ((,class (:foreground ,color-headline-4 :bold t :underline (:color ,grey-3) :extend t))))
  `(outshine-level-5 ((,class (:foreground ,color-headline-1 :bold t :underline (:color ,grey-3) :extend t))))
  `(outshine-level-6 ((,class (:foreground ,color-headline-2 :bold t :underline (:color ,grey-3) :extend t))))
  `(outshine-level-7 ((,class (:foreground ,color-headline-3 :bold t :underline (:color ,grey-3) :extend t))))
  `(outshine-level-8 ((,class (:foreground ,color-headline-4 :bold t :underline (:color ,grey-3) :extend t))))

  ;; Magit faces
  `(magit-section-highlight ((,class (:background ,color-highlight-1))))
  `(magit-section-heading ((,class (:foreground ,pink-2 :weight bold))))
  `(magit-hash ((,class (:foreground ,grey-4))))
  `(magit-dimmed ((,class (:foreground ,grey-4))))
  `(magit-signature-error ((,class (:foreground ,red-1))))
  `(magit-log-author ((,class (:foreground ,red-1))))
  `(magit-log-date ((,class (:foreground ,grey-4))))
  `(magit-log-graph ((,class (:foreground ,grey-4))))
  `(magit-diff-added ((,class (:background "#3e4d4b" :foreground ,white-1))))
  `(magit-diff-added-highlight ((,class (:background ,color-diff-added :foreground ,white-1))))
  `(magit-diff-removed ((,class (:background "#3d373d" :foreground ,white-1))))
  `(magit-diff-removed-highlight ((,class (:background ,color-diff-removed :foreground ,white-1))))
  `(magit-diff-hunk-heading ((,class (:background ,grey-2 :foreground ,grey-4))))
  `(magit-diff-hunk-heading-highlight ((,class (:background ,grey-3 :foreground ,white-1))))
  `(magit-diff-context ((,class (:foreground ,grey-3))))
  `(magit-diff-context-highlight ((,class (:background ,grey-2 :foreground ,white-1))))
  `(magit-head ((,class (:foreground ,blue-2 :weight bold))))
  `(magit-branch-local ((,class (:foreground ,blue-2 :weight bold))))
  `(magit-branch-remote ((,class (:foreground ,green-2 :weight bold))))
  `(magit-branch-remote-head ((,class (:box (:line-width 1) :foreground ,green-2))))
  `(magit-branch-current ((,class (:box (:line-width 1) :foreground ,blue-2))))
  `(magit-tag ((,class (:foreground ,yellow-1 :weight bold))))
  `(magit-header-line ((,class nil)))
  `(magit-blame-highlight ((,class (:foreground ,color-comment :background ,grey-2))))
  `(git-commit-summary ((,class (:foreground ,white-1))))

  ;; git-gutter
  `(git-gutter:added ((,class (:foreground ,green-2 :bold t))))
  `(git-gutter:deleted ((,class (:foreground ,red-1 :bold t))))
  `(git-gutter:modified ((,class (:foreground ,color-comment :bold t))))

  ;; debugging
  `(breakpoint-enabled ((,class (:foreground ,red-1))))
  `(breakpoint-disabled ((,class (:foreground ,color-comment))))

  ;; realgud
  `(realgud-bp-line-enabled-face ((,class ())))

  ;; Elfeed faces
  `(message-header-name ((,class (:foreground ,grey-3 :weight bold))))
  `(message-header-subject ((,class (:foreground ,blue-2 :weight bold))))
  `(message-header-to ((,class (:foreground ,grey-4 :weight bold))))
  `(message-header-other ((,class (:foreground ,pink-2))))
  `(message-header-other ((,class (:foreground ,pink-2))))
  `(elfeed-search-date-face ((,class (:foreground ,color-headline-1 :weight bold))))
  `(elfeed-search-tag-face ((,class (:foreground ,color-comment))))
  `(elfeed-search-feed-face ((,class (:foreground ,color-headline-2 :weight bold))))
  `(elfeed-search-unread-title-face ((,class (:foreground ,color-foreground))))
  `(elfeed-search-title-face ((,class (:foreground ,color-comment))))

  ;; Neotree
  `(neo-dir-link-face ((,class (:foreground ,color-link :weight bold))))
  `(neo-root-dir-face ((,class (:foreground ,color-headline-1 :weight bold))))

  ;; Treemacs
  `(treemacs-root-face ((,class (:foreground ,color-foreground :weight bold))))
  `(treemacs-file-face ((,class (:foreground ,color-foreground))))
  `(treemacs-git-ignored-face ((,class (:foreground ,color-foreground))))
  `(treemacs-git-modified-face ((,class (:foreground ,color-foreground))))
  `(treemacs-git-added-face ((,class (:foreground ,color-foreground))))
  `(treemacs-git-untracked-face ((,class (:foreground ,color-foreground))))
  `(treemacs-directory-face ((,class (:foreground ,color-foreground))))
  `(treemacs-pulse-on-success ((,class (:background ,color-success))))
  `(treemacs-pulse-on-error ((,class (:background ,color-error))))

  `(chip-theme-treemacs-root-face ((,class (:foreground ,color-foreground))))
  `(chip-theme-treemacs-file-face ((,class (:foreground ,color-foreground))))

  ;; Calfw
  `(cfw:face-title ((,class (:foreground ,color-foreground))))
  `(cfw:face-toolbar ((,class (:background ,color-background))))
  `(cfw:face-toolbar-button-off ((,class (:background ,color-background :foreground ,color-comment))))
  `(cfw:face-toolbar-button-on ((,class (:background ,color-background :foreground ,blue-2))))
  `(cfw:face-grid ((,class (:foreground ,color-highlight-1))))
  `(cfw:face-select ((,class (:background ,color-highlight-1))))
  `(cfw:face-day-title ((,class (:background ,color-background))))
  `(cfw:face-today-title ((,class (:background ,color-highlight-1))))
  `(cfw:face-holiday ((,class (:foreground ,color-foreground))))
  `(cfw:face-sunday ((,class (:foreground ,color-foreground))))
  `(cfw:face-disable ((,class (:foreground ,color-comment))))
  `(cfw:face-header ((,class (:foreground ,color-foreground))))

  ;; Lispy
  `(lispy-face-hint ((,class (:foreground ,grey-4 :weight bold))))

  ;; CIDER
  `(cider-test-success-face ((,class (:foreground ,color-success :weight bold))))
  `(cider-test-failure-face ((,class (:background ,color-error   :foreground ,white-1 :weight bold))))
  `(cider-fringe-good-face ((,class (:foreground ,color-comment))))

  ;; SLIME faces
  `(slime-repl-inputed-output-face ((,class (:foreground ,color-comment))))

  ;; Sly faces
  `(sly-mrepl-output-face ((,class (:foreground ,color-comment))))
  `(sly-mrepl-note-face ((,class (:foreground ,color-comment))))
  `(sly-mrepl-prompt-face ((,class (:foreground ,color-foreground :weight bold))))
  `(sly-part-button-face ((,class (:foreground ,blue-2 :weight bold))))

  ;; Macrostep faces
  `(macrostep-expansion-highlight-face ((,class (:background ,color-highlight-1 :extend t))))

  ;; Slack faces
  `(slack-message-output-header ((,class (:foreground ,blue-2 :weight bold))))
  `(slack-message-output-reaction ((,class (:foreground ,grey-4))))
  `(lui-button-face ((,class (:foreground ,blue-2 :underline t))))
  `(lui-time-stamp-face ((,class (:foreground ,pink-2 :weight bold))))

  ;; Erc faces
  `(erc-nick-default-face ((,class (:foreground ,green-1 :weight bold))))
  `(erc-current-nick-face ((,class (:foreground ,green-1 :weight bold))))
  `(erc-notice-face ((,class (:foreground ,grey-3))))
  `(erc-input-face ((,class (:foreground ,white-1))))
  `(erc-timestamp-face ((,class (:foreground ,grey-3 :weight bold))))
  `(erc-prompt-face ((,class (:foreground ,grey-3 :weight bold))))
  `(erc-nick-msg-face ((,class (:foreground ,red-1))))
  `(erc-direct-msg-face ((,class (:foreground ,red-2))))

  ;; EIN
  `(ein:cell-input-area ((,class (:background ,grey-2))))
  `(ein:cell-output-area ((,class (:foreground ,grey-4))))

  ;; notmuch
  `(notmuch-search-unread-face ((,class (:weight bold))))
  `(notmuch-message-summary-face ((,class (:background ,grey-2))))
  `(notmuch-tag-unread ((,class (:foreground ,red-2))))
  `(notmuch-tag-face ((,class (:foreground ,green-2))))
  `(notmuch-show-part-button-default ((,class (:inherit link))))
  `(message-mml ((,class (:inherit link))))
  `(widget-field ((,class (:background ,grey-3))))
  `(widget-button ((,class (:inherit link))))
  `(widget-button-pressed ((,class (:background ,grey-2 :foreground ,color-foreground))))

  ;; Pomidor
  `(pomidor-time-face ((,class (:foreground ,green-1 :weight bold))))
  `(pomidor-timer-face ((,class (:foreground ,green-1 :weight bold))))
  `(pomidor-break-face ((,class (:foreground ,blue-2 :weight bold))))
  `(pomidor-work-face ((,class (:foreground ,green-1 :weight bold))))
  `(pomidor-overwork-face ((,class (:foreground ,red-2 :weight bold))))

  ;; Escape and prompt faces
  `(minibuffer-prompt ((,class (:weight bold :foreground ,grey-3))))
  `(escape-glyph ((,class (:foreground ,red-2))))
  `(homoglyph ((,class (:foreground ,red-2))))
  `(error ((,class (:foreground ,color-error))))
  `(warning ((,class (:foreground ,color-error))))
  `(success ((,class (:foreground ,color-success))))

  ;; Font lock faces
  `(font-lock-builtin-face ((,class (:foreground ,pink-2))))
  `(font-lock-comment-face ((,class (:foreground ,color-comment))))
  `(font-lock-constant-face ((,class (:foreground ,blue-2))))
  `(font-lock-function-name-face ((,class (:foreground ,pink-2))))
  `(font-lock-keyword-face ((,class (:foreground ,blue-2))))
  `(font-lock-string-face ((,class (:foreground ,green-2))))
  `(font-lock-type-face ((,class (:foreground ,red-2))))
  `(font-lock-variable-name-face ((,class (:foreground ,pink-2))))

  ;; Markdown faces
  `(markdown-markup-face ((,class (:foreground ,grey-3))))

  ;; Message faces
  `(message-header-name ((,class (:foreground ,blue-2))))
  `(message-header-cc ((,class (:foreground ,yellow-1))))
  `(message-header-other ((,class (:foreground ,red-2))))
  `(message-header-subject ((,class (:foreground ,red-2))))
  `(message-header-to ((,class (:weight bold :foreground ,yellow-1))))
  `(message-cited-text ((,class (:slant italic :foreground ,grey-4))))
  `(message-separator ((,class (:weight bold :foreground ,green-2))))

  ;; Tabbar faces
  `(tabbar-default
    ((,class (:background ,red-1 :foreground ,blue-1)))))

(provide-theme 'chip)
