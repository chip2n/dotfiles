(in-package :next)

(add-to-default-list 'vi-normal-mode 'buffer 'default-modes)

(defvar *my-keymap* (make-keymap))

(define-key :keymap *my-keymap*
  "C-d" #'scroll-page-down
  "C-u" #'scroll-page-up)

(define-mode my-mode ()
  ""
  ((keymap-schemes :initform (list :emacs-map *my-keymap*
                                   :vi-normal *my-keymap*))))

(add-to-default-list 'my-mode 'buffer 'default-modes)
