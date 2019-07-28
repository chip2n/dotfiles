(provide 'init-elfeed)

(use-package elfeed
  :ensure t
  :config
  (setq shr-inhibit-images t)           ; disable image loading when viewing entries
  (setq elfeed-feeds
        '(("http://feeds.feedburner.com/blogspot/hsDu" android) ; Android Developers Blog
	  ("http://oremacs.com/atom.xml" emacs)                 ; (or emacs)
	  ("http://pragmaticemacs.com/feed/" emacs)             ; Pragmatic Emacs
          ("https://emacsair.me/feed.xml" emacs)                ; Tarsius blog
          ("http://feeds.bbci.co.uk/news/science_and_environment/rss.xml" news) ; BBC News - Science & Environment
          ("https://www.theverge.com/rss/index.xml" news) ; The Verge
          ("https://defn.io/index.xml" racket)
          "http://techsnuffle.com/feed.xml")))
