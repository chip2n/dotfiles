(provide 'init-google-translate)

(use-package google-translate
  :ensure t
  :config
  (require 'google-translate-default-ui)

  ;; workaround for issue #52
  ;; https://github.com/atykhonov/google-translate/issues/52
  (defun google-translate--search-tkk () "Search TKK." (list 430675 2721866130))

  ;; insert translations in current buffer
  (setq google-translate-output-destination 'current-buffer))
