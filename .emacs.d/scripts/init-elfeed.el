(provide 'init-elfeed)

(use-package elfeed
  :ensure t)

(setq elfeed-feeds
      '("http://feeds.feedburner.com/blogspot/hsDu"
	"http://oremacs.com/atom.xml"
	"http://pragmaticemacs.com/feed/"
        ))
