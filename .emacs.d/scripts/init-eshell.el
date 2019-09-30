(provide 'init-eshell)

(add-hook 'eshell-mode-hook (lambda () (display-line-numbers-mode -1)))
(setenv "ANDROID_SDK_ROOT" "/home/chip/Android/Sdk")
