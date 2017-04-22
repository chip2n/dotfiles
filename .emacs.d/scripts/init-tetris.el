(provide 'init-tetris)

(defvar tetris-mode-map
  (make-sparse-keymap 'tetris-mode-map))

(define-key tetris-mode-map "n"     'tetris-start-game)
(define-key tetris-mode-map "q"     'tetris-end-game)
(define-key tetris-mode-map "p"     'tetris-pause-game)
(define-key tetris-mode-map " "     'tetris-move-bottom)
(define-key tetris-mode-map "h"  'tetris-move-left)
(define-key tetris-mode-map "l" 'tetris-move-right)
(define-key tetris-mode-map "k"    'tetris-rotate-prev)
(define-key tetris-mode-map "j"  'tetris-rotate-next)
