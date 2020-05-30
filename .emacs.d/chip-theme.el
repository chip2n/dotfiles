(deftheme chip
  "Face colors using the chip palette.")

(defvar colors-dark
  '((white-1   . "#eceff6")
    (grey-0    . "#1b1e24")
    (grey-1    . "#21242b")
    (grey-2    . "#282c34")
    (grey-3    . "#494e5a")
    (grey-4    . "#6b7385")
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
    (color-modeline-active   . grey-2)
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
    (color-diff-added        . "#556f55")
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
    (color-diff-added        . "#556f55")
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
                                         ,blue-2 ,magenta-2 ,blue-1 ,grey-4]))))

(with-theme 'chip
  `(default ((,class (:foreground ,color-foreground :background ,color-background :family "Iosevka" :height 100))))
  `(fixed-pitch ((,class (:family "Iosevka" :height 100))))
  `(variable-pitch ((,class (:family "Iosevka" :height 100))))
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
  `(link ((,class (:foreground ,blue-2 :underline t))))
  `(link-visited ((,class (:foreground ,blue-1 :underline t))))
  `(line-number ((,class (:foreground ,color-comment))))

  ;; Paren match
  `(show-paren-match ((,class (:background ,color-highlight-2))))

  ;; Header faces
  `(header-line ((,class (:box (:line-width 4 :color ,color-background :style nil) :background ,color-background :foreground ,color-foreground))))

  ;; Mode line faces
  `(mode-line ((,class (:box () :background ,color-modeline-active :foreground ,color-foreground))))
  `(mode-line-inactive ((,class (:box () :background ,color-modeline-inactive :foreground ,grey-3))))
  `(mode-line-evil-state-normal ((,class (:foreground ,color-evil-normal))))
  `(mode-line-evil-state-insert ((,class (:foreground ,color-evil-insert))))
  `(mode-line-evil-state-visual ((,class (:foreground ,color-evil-visual))))
  `(mode-line-evil-state-motion ((,class (:foreground ,color-evil-motion))))
  `(mode-line-evil-state-emacs ((,class (:foreground ,color-evil-emacs))))

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

  ;; Term colors
  `(term-color-black ((,class (:foreground ,grey-3))))
  `(term-color-red ((,class (:foreground ,red-2))))
  `(term-color-blue ((,class (:foreground ,blue-2))))
  `(term-color-green ((,class (:foreground ,green-2))))
  `(term-color-yellow ((,class (:foreground ,yellow-2))))
  `(term-color-pink ((,class (:foreground ,pink-2))))
  `(term-color-magenta ((,class (:foreground ,magenta-2))))

  ;; Ivy
  `(ivy-virtual ((,class (:foreground ,white-1))))

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

  ;; Avy
  `(avy-lead-face ((,class (:background ,red-1 :foreground ,grey-2 :weight bold))))
  `(avy-lead-face-0 ((,class (:background ,red-1 :foreground ,grey-2 :weight bold))))

  ;; Org
  `(org-ellipsis ((,class (:foreground ,grey-4))))
  `(org-todo ((,class (:foreground ,red-2 :weight bold))))
  `(org-done ((,class (:foreground ,green-2 :weight bold))))
  `(org-level-1 ((,class (:foreground ,color-headline-1 :weight bold))))
  `(org-level-2 ((,class (:foreground ,color-headline-2 :weight bold))))
  `(org-level-3 ((,class (:foreground ,color-headline-3 :weight bold))))
  `(org-level-4 ((,class (:foreground ,color-headline-4 :weight bold))))
  `(org-level-5 ((,class (:foreground ,color-headline-1 :weight bold))))
  `(org-level-6 ((,class (:foreground ,color-headline-2 :weight bold))))
  `(org-level-7 ((,class (:foreground ,color-headline-3 :weight bold))))
  `(org-level-8 ((,class (:foreground ,color-headline-4 :weight bold))))
  `(org-link ((,class (:foreground ,color-link :underline t))))
  `(org-date ((,class (:foreground ,blue-2 :underline t))))
  `(org-tag ((,class (:foreground ,color-comment))))
  `(org-upcoming-deadline ((,class (:foreground ,grey-4))))
  `(org-upcoming-distant-deadline ((,class (:foreground ,grey-4))))
  `(org-warning ((,class (:foreground ,color-error))))
  `(org-code ((,class (:foreground ,pink-2 :weight bold :inherit fixed-pitch))))
  `(org-verbatim ((,class (:foreground ,pink-2 :weight bold :inherit fixed-pitch))))
  `(org-block-begin-line ((,class (:foreground ,color-comment :inherit fixed-pitch :background ,color-highlight-1 :extend t))))
  `(org-block ((,class (:foreground ,color-foreground :inherit fixed-pitch :background ,grey-0 :extend t))))
  `(org-block-end-line ((,class (:foreground ,color-comment :inherit fixed-pitch :background ,color-highlight-1 :extend t))))

  ;; Org bullets
  `(org-bullet ((,class (:foreground ,blue-2 :weight bold))))
  `(org-superstar-leading ((,class (:foreground ,blue-2 :weight bold))))
  `(org-superstar-header-bullet ((,class (:foreground ,blue-2 :weight bold))))

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
  `(outshine-level-1 ((,class (:foreground ,color-headline-1 :underline t))))
  `(outshine-level-2 ((,class (:foreground ,color-headline-2 :underline t))))
  `(outshine-level-3 ((,class (:foreground ,color-headline-3 :underline t))))
  `(outshine-level-4 ((,class (:foreground ,color-headline-4 :underline t))))
  `(outshine-level-5 ((,class (:foreground ,color-headline-1 :underline t))))
  `(outshine-level-6 ((,class (:foreground ,color-headline-2 :underline t))))
  `(outshine-level-7 ((,class (:foreground ,color-headline-3 :underline t))))
  `(outshine-level-8 ((,class (:foreground ,color-headline-4 :underline t))))

  ;; Magit faces
  `(magit-section-highlight ((,class (:background ,color-highlight-1))))
  `(magit-section-heading ((,class (:foreground ,pink-2 :weight bold))))
  `(magit-hash ((,class (:foreground ,grey-4))))
  `(magit-dimmed ((,class (:foreground ,grey-4))))
  `(magit-signature-error ((,class (:foreground ,red-1))))
  `(magit-log-author ((,class (:foreground ,red-1))))
  `(magit-log-date ((,class (:foreground ,grey-4))))
  `(magit-log-graph ((,class (:foreground ,grey-4))))
  `(magit-diff-added ((,class (:background ,color-diff-added :foreground ,white-1))))
  `(magit-diff-added-highlight ((,class (:background ,color-diff-added :foreground ,white-1))))
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
  `(git-commit-summary ((,class (:foreground ,white-1))))

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

  ;; Lispy
  `(lispy-face-hint ((,class (:foreground ,grey-4 :weight bold))))

  ;; CIDER
  `(cider-test-success-face ((,class (:foreground ,color-success :weight bold))))
  `(cider-test-failure-face ((,class (:background ,color-error   :foreground ,white-1 :weight bold))))
  `(cider-fringe-good-face ((,class (:foreground ,color-comment))))

  ;; SLIME faces
  `(slime-repl-inputed-output-face ((,class (:foreground ,red-1))))

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
