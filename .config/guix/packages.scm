;; Pass this manifest file to 'guix package -m' to install all packages

(specifications->manifest
 (list "vim"
       "emacs"
       "git"
       "sbcl"))

;; TODO lemonbar-xft
;; TODO stumpwm
