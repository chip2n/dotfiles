(deftheme chip
  "Face colors using the chip palette (dark background).")

(let* ((class '((class color) (min-colors 89)))
      ;; Chip palette colors.
      (white-1 "#ffffff")
      (grey-1 "#21242b") (grey-2 "#282c34") (grey-3 "#3c404a") (grey-4 "#494e5a")

      (red-1 "#d98077") (red-2 "#f1988e")
      (green-1 "#89d2b0") (green-2 "#b9e59f")
      (yellow-1 "#eae1a6") (yellow-2 "#fbf2bf")
      (blue-1 "#67acd5") (blue-2 "#96d3e8")
      (pink-1 "#dc8cac") (pink-2 "#ecb3c9")
      (magenta-1 "#dc8cac") (magenta-2 "#ecb3c9")

      (highlight grey-2)
      (diff-added "#556f55")

      ;; Tango palette colors.
      (butter-1 "#fce94f") (butter-2 "#edd400") (butter-3 "#c4a000")
      (orange-1 "#fcaf3e") (orange-2 "#f57900") (orange-3 "#ce5c00")
      (choc-1 "#e9b96e") (choc-2 "#c17d11") (choc-3 "#8f5902")
      (cham-1 "#8ae234") (cham-2 "#73d216") (cham-3 "#4e9a06")
      (blue-3 "#204a87")
      (plum-1 "#ad7fa8") (plum-2 "#75507b") (plum-3 "#5c3566")
      (red-3 "#a40000")
      (alum-1 "#eeeeec") (alum-2 "#d3d7cf") (alum-3 "#babdb6")
      (alum-4 "#888a85") (alum-5 "#5f615c") (alum-6 "#2e3436")
      ;; Not in Tango palette; used for better contrast.
      (cham-4 "#346604") (blue-0 "#8cc4ff") (orange-4 "#b35000"))

  (custom-theme-set-faces
   'chip
   `(default ((,class (:foreground ,white-1 :background ,grey-1))))
   `(cursor ((,class (:background ,grey-4))))

   ;; Highlighting faces
   `(fringe ((,class (:foreground ,grey-3 :background ,grey-1))))
   `(highlight ((,class (:background ,highlight))))
   `(region ((,class (:background ,grey-3))))
   `(secondary-selection ((,class (:background ,grey-2))))
   `(isearch ((,class (:foreground ,white-1 :background ,blue-1))))
   `(lazy-highlight ((,class (:background ,grey-3))))
   `(trailing-whitespace ((,class (:background ,red-2))))
   `(vertical-border ((,class (:foreground ,grey-2))))
   `(link ((,class (:foreground ,blue-2 :underline t))))
   `(link-visited ((,class (:foreground ,blue-1 :underline t))))
   `(line-number ((,class (:foreground ,grey-3))))

   ;; Paren match
   `(show-paren-match
     ((,class (:background ,grey-4))))

   ;; Header faces
   `(header-line ((,class (:box ()
			 :background ,grey-2 :foreground ,white-1))))

   ;; Mode line faces
   `(mode-line ((,class (:box ()
			 :background ,grey-2 :foreground ,white-1))))
   `(mode-line-inactive ((,class (:box ()
				  :background ,grey-2 :foreground ,grey-3))))

   ;; Term colors
   `(term-color-black ((,class (:foreground ,grey-3))))
   `(term-color-red ((,class (:foreground ,red-2))))
   `(term-color-blue ((,class (:foreground ,blue-2))))
   `(term-color-green ((,class (:foreground ,green-2))))
   `(term-color-yellow ((,class (:foreground ,yellow-2))))
   `(term-color-pink ((,class (:foreground ,pink-2))))
   `(term-color-magenta ((,class (:foreground ,magenta-2))))

   ;; Org
   `(org-ellipsis ((,class (:foreground ,grey-4))))
   `(org-todo ((,class (:foreground ,red-2 :weight bold))))
   `(org-done ((,class (:foreground ,green-2 :weight bold))))
   `(org-level-1 ((,class (:foreground ,red-2 :weight bold))))
   `(org-level-2 ((,class (:foreground ,blue-2 :weight bold))))
   `(org-level-3 ((,class (:foreground ,pink-2 :weight bold))))
   `(org-level-4 ((,class (:foreground ,green-2 :weight bold))))
   `(org-level-5 ((,class (:foreground ,red-2 :weight bold))))
   `(org-level-6 ((,class (:foreground ,blue-2 :weight bold))))
   `(org-level-7 ((,class (:foreground ,pink-2 :weight bold))))
   `(org-level-8 ((,class (:foreground ,green-2 :weight bold))))

   ;; Magit faces
   `(magit-section-highlight ((,class (:background ,highlight))))
   `(magit-section-heading ((,class (:foreground ,yellow-1 :weight bold))))
   `(magit-hash ((,class (:foreground ,grey-4))))
   `(magit-dimmed ((,class (:foreground ,grey-4))))
   `(magit-signature-error ((,class (:foreground ,red-1))))
   `(magit-log-author ((,class (:foreground ,red-1))))
   `(magit-log-date ((,class (:foreground ,grey-4))))
   `(magit-log-graph ((,class (:foreground ,grey-4))))
   `(magit-diff-added ((,class (:background ,diff-added :foreground ,white-1))))
   `(magit-diff-added-highlight ((,class (:background ,diff-added :foreground ,white-1))))
   `(magit-diff-hunk-heading ((,class (:background ,grey-2 :foreground ,grey-4))))
   `(magit-diff-hunk-heading-highlight ((,class (:background ,grey-3 :foreground ,white-1))))
   `(magit-diff-context ((,class (:foreground ,grey-3))))
   `(magit-diff-context-highlight ((,class (:background ,grey-2 :foreground ,white-1))))
   `(magit-head ((,class (:foreground ,blue-2 :weight bold))))
   `(magit-branch-local ((,class (:foreground ,blue-2 :weight bold))))
   `(magit-branch-remote ((,class (:foreground ,green-2 :weight bold))))
   `(magit-branch-remote-head ((,class (:box (:line-width 1)
					     :foreground ,green-2))))
   `(magit-branch-current ((,class (:box (:line-width 1)
					     :foreground ,blue-2))))
   `(git-commit-summary ((,class (:foreground ,white-1))))

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
   `(erc-timestamp-face ((,class (:foreground ,pink-2 :weight bold))))
   `(erc-prompt-face ((,class (:foreground ,grey-3 :weight bold))))
   `(erc-nick-msg-face ((,class (:foreground ,red-1))))
   `(erc-direct-msg-face ((,class (:foreground ,red-2))))

   ;; EIN
   `(ein:cell-input-area ((,class (:background ,grey-3))))

   ;; Escape and prompt faces
   `(minibuffer-prompt ((,class (:weight bold :foreground ,grey-3))))
   `(escape-glyph ((,class (:foreground ,red-3))))
   `(homoglyph ((,class (:foreground ,red-3))))
   `(error ((,class (:foreground ,red-2))))
   `(warning ((,class (:foreground ,orange-3))))
   `(success ((,class (:foreground ,cham-3))))

   ;; Font lock faces
   `(font-lock-builtin-face ((,class (:foreground ,pink-2))))
   `(font-lock-comment-face ((,class (:foreground ,grey-4))))
   `(font-lock-constant-face ((,class (:foreground ,blue-2))))
   `(font-lock-function-name-face ((,class (:foreground ,pink-2))))
   `(font-lock-keyword-face ((,class (:foreground ,blue-2))))
   `(font-lock-string-face ((,class (:foreground ,green-2))))
   `(font-lock-type-face ((,class (:foreground ,red-2))))
   `(font-lock-variable-name-face ((,class (:foreground ,pink-2))))

   ;; Markdown faces
   `(markdown-markup-face ((,class (:foreground ,grey-3))))

   ;; Telephone line faces
   `(telephone-line-evil-normal
     ((,class (:background ,grey-2 :foreground ,white-1))))
   `(telephone-line-evil-insert
     ((,class (:background ,blue-2 :foreground ,white-1))))
   `(telephone-line-evil-visual
     ((,class (:background ,blue-2 :foreground ,white-1))))
   `(telephone-line-evil-emacs
     ((,class (:background ,pink-1 :foreground ,white-1))))
   `(telephone-line-evil-motion
     ((,class (:background ,pink-1 :foreground ,white-1))))
   `(telephone-line-accent-active
     ((,class (:background ,grey-3 :foreground ,white-1))))
   `(telephone-line-accent-inactive
     ((,class (:background ,grey-2 :foreground ,grey-3))))

   ;; Button and link faces
   `(link ((,class (:underline t :foreground ,blue-3))))
   `(link-visited ((,class (:underline t :foreground ,blue-2))))

   ;; Message faces
   `(message-header-name ((,class (:foreground ,blue-3))))
   `(message-header-cc ((,class (:foreground ,butter-3))))
   `(message-header-other ((,class (:foreground ,choc-2))))
   `(message-header-subject ((,class (:foreground ,red-3))))
   `(message-header-to ((,class (:weight bold :foreground ,butter-3))))
   `(message-cited-text ((,class (:slant italic :foreground ,alum-5))))
   `(message-separator ((,class (:weight bold :foreground ,cham-3)))))

   ;; Tabbar faces
   `(tabbar-default
     ((,class (:background ,red-1 :foreground ,blue-1))))


  (custom-theme-set-variables
   'chip
   `(ansi-color-names-vector [,alum-6 ,red-3 ,cham-3 ,butter-3
				      ,blue-3 ,plum-3 ,blue-1 ,alum-1])))

(provide-theme 'chip)
