(defun android-screenshot (path)
  "Take a screenshot of the currently connected Android device."
  (interactive "FPath: ")
  (shell-command (format "droidscrot %s" path)))

(defun android-screenshot-shrinked (path)
  "Take a screenshot of the currently connected Android device and resize it to fit org-reveal."
  (interactive "FPath: ")
  (android-screenshot path)
  (shell-command (format "shrink_image.sh %s" path)))

(defun android-screenshot-shrinked-with-link (path)
  "Take a shrinked screenshot and insert link."
  (interactive "FPath: ")
  (android-screenshot-shrinked path)
  (org-insert-link nil (format "file:%s" path) nil))

(provide 'init-android)
