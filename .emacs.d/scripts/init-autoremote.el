(defun autoremote-send (message)
  (if (boundp 'autoremote-api-key)
      (call-process-shell-command
       (format "AUTOREMOTE_API_KEY=\"%s\" autoremote %s" autoremote-api-key message))
    (message (format "No autoremote key set - unable to send message \"%s\"" message))))

(provide 'init-autoremote)
