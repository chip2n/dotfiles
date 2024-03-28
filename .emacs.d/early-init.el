(defconst c/mac? (equal system-type 'darwin)
  "Returns t if running on a mac.")

;; Prevent package.el from loading packages (straight is handling this)
(setq package-enable-at-startup nil)

;; NOTE: Disabling menu bar seems to break fullscreen mode. But the menu isn't
;; visible in the frame anyways
(unless c/mac?
  (menu-bar-mode -1))

(unless c/mac?
  (tool-bar-mode -1))
