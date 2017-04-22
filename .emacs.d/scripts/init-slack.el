(provide 'init-slack)
(require 'private)

(use-package slack
  :commands (slack-start)
  :init
  (setq slack-buffer-emojify t)
  (setq slack-prefer-current-team t)
  :ensure t
  :config
  (slack-register-team
   :name "remente"
   :default t
   :client-id private/slack-client-id-remente
   :client-secret private/slack-client-secret-remente
   :token private/slack-token-remente))
