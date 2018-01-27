(deftheme chip
  "Face colors using the chip palette (dark background).")

(let* ((class '((class color) (min-colors 89)))
      ;; Chip palette colors.
      (white-1 "#ffffff")
      (grey-1 "#21242b") (grey-2 "#282c34") (grey-3 "#3c404a")
      (blue-1 "#3a8ba6") (blue-2 "#67acd5")
      (red-1 "#f28479") (red-2 "#fd8a7e")
      (green-1 "#7fdd98") (green-2 "#85e79f")
      (pink-1 "#f49bbe") (pink-2 "#ffa2c6")

      (highlight grey-2)

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
   `(cursor ((,class (:background ,grey-3))))

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

   ;; Header faces
   `(header-line ((,class (:box ()
			 :background ,grey-2 :foreground ,white-1))))

   ;; Mode line faces
   `(mode-line ((,class (:box ()
			 :background ,grey-2 :foreground ,white-1))))
   `(mode-line-inactive ((,class (:box ()
				  :background ,grey-2 :foreground ,grey-3))))

   ;; Magit faces
   `(magit-section-highlight ((,class (:background ,highlight))))

   ;; Escape and prompt faces
   `(minibuffer-prompt ((,class (:weight bold :foreground ,grey-3))))
   `(escape-glyph ((,class (:foreground ,red-3))))
   `(homoglyph ((,class (:foreground ,red-3))))
   `(error ((,class (:foreground ,red-2))))
   `(warning ((,class (:foreground ,orange-3))))
   `(success ((,class (:foreground ,cham-3))))

   ;; Font lock faces
   `(font-lock-builtin-face ((,class (:foreground ,pink-1))))
   `(font-lock-comment-face ((,class (:foreground ,grey-3))))
   `(font-lock-constant-face ((,class (:foreground ,blue-2))))
   `(font-lock-function-name-face ((,class (:foreground ,pink-1))))
   `(font-lock-keyword-face ((,class (:foreground ,green-1))))
   `(font-lock-string-face ((,class (:foreground ,green-2))))
   `(font-lock-type-face ((,class (:foreground ,red-1))))
   `(font-lock-variable-name-face ((,class (:foreground ,pink-1))))

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

  (custom-theme-set-variables
   'chip
   `(ansi-color-names-vector [,alum-6 ,red-3 ,cham-3 ,butter-3
				      ,blue-3 ,plum-3 ,blue-1 ,alum-1])))

(provide-theme 'chip)