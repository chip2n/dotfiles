(require 'request)

(defvar tayl-api-token nil)

(defun tayl-send (url)
  (if (not tayl-api-token)
      (message "You need to specify an API token for Tayl first (using tayl-api-token).")
    (tayl--send-with-token url tayl-api-token)))

(defun tayl--send-with-token (url token)
  (message "Sending %S to Tayl..." url)
  (request
   "https://x.tayl.app/submit"
   :type "POST"
   :headers `(("ContentType" . "application/json")
              ("x-api-token" . ,token))
   :data `(("url" . ,url))
   :parser 'json-read
   :success (cl-function
             (lambda (&key data &allow-other-keys)
               (message "Success!")))
   :error (cl-function
           (lambda (&key error-thrown &allow-other-keys)
             (message "Error: %S" error-thrown)))
   :complete (lambda (&rest _)
               (message "Finished!"))))

(provide 'tayl)
