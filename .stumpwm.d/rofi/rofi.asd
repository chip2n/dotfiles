;;;; rofi.asd

(asdf:defsystem #:rofi
  :description "Interface to rofi"
  :author "Andreas Arvidsson (andreas@arvidsson.io)"
  :license  "GPLv3"
  :version "0.0.1"
  :serial t
  :depends-on (#:stumpwm #:uiop #:str)
  :components ((:file "package")
               (:file "rofi")))
