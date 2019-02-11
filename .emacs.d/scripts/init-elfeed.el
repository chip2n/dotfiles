(provide 'init-elfeed)

(use-package elfeed
  :ensure t
  :config
  (setq shr-inhibit-images t)           ;; disable image loading when viewing entries
  (setq elfeed-feeds
        '(("http://feeds.feedburner.com/blogspot/hsDu" android) ;; Android Developers Blog
	  ("http://oremacs.com/atom.xml" emacs)                 ;; (or emacs)
	  ("http://pragmaticemacs.com/feed/" emacs)             ;; Pragmatic Emacs
          ("http://planet.emacsen.org/atom.xml" emacs)          ;; Planet Emacsen
          ("http://feeds.bbci.co.uk/news/science_and_environment/rss.xml" news) ;; BBC News - Science & Environment
          ("https://www.theverge.com/rss/index.xml" news) ;; The Verge
          )))

(require 'tayl)

(defun chip/elfeed-send-to-tayl ()
  (interactive)
  (let ((url (chip/elfeed-get-url-of-current-feed)))
    (tayl-send url)))

(defun chip/elfeed-get-url-of-current-feed ()
  (save-excursion
    (beginning-of-buffer)
    (re-search-forward "Link: " nil t)
    (elfeed-get-url-at-point)))
