(provide 'init-org-reveal)

;; (use-package ox-reveal
;;   :ensure t
;;   :config
;;   (setq org-reveal-root "http://cdn.jsdelivr.net/reveal.js/3.0.0/"))

;; org-reveal does not seem to be maintained any longer:
;; https://github.com/yjwen/org-reveal/issues/363

;; switching to org-re-reveal (a maintained fork)
;; https://gitlab.com/oer/org-re-reveal

(use-package org-re-reveal
  :ensure t
  :config
  (setq org-re-reveal-root "file:///home/chip/reveal.js")
  (setq org-re-reveal-title-slide nil))
