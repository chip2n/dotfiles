;; Pass this manifest file to 'guix package -m' to install all packages

(specifications->manifest
 (list
  ;; applications
  "emacs"
  "git"
  "st"
  ;; languages
  "sbcl"
  "zig"
  ;; utils
  "direnv"))
