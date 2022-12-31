;;;; rofi.lisp

(in-package #:rofi)

(defun interleave (a b)
  (when (and a b)
    (cons (car a) (cons (car b) (interleave (cdr a) (cdr b))))))

(defun normalize-items (items)
  (loop :for item :in items
        :collect (typecase item
                   (list item)
                   (t (list item)))))

(defun longest-in-column (items col)
  (loop :for item :in items
        :if (typep item 'list)
          :maximize (length (nth col item))))

(defun format-items (items &key (padding 5))
  (let* ((items (normalize-items items))
         (num-cols (loop :for item :in items :maximize (length item)))
         (column-max-lengths (loop :for col :from 0 :below num-cols
                                   :collect (+ padding (longest-in-column items col)))))
    (loop :for item :in items
          :collect (format nil "~{~va~}" (interleave column-max-lengths item))))
  ;; (loop for item in items
  ;;       collect (format nil "" ))
  )

(defun run (items &key (prompt "'>'"))
  ;; (setf items
  ;;       (loop for item in items
  ;;             collect (etypecase item
  ;;                       (list (str:join #\tab item))
  ;;                       (t item))))

  (let* ((s (make-string-input-stream (format nil "~{~a~%~}" (format-items items)))))
    (multiple-value-bind (index _ status) (uiop:run-program
                                           (format nil "rofi -dmenu -i -p '~a' -format 'i'" prompt)
                                           :ignore-error-status t
                                           :input s
                                           :output :string)
      (declare (ignore _))
      (when (= status 0)
        (let ((idx (parse-integer (str:trim-right index))))
          (when (>= 0) (values (nth idx items) idx)))))))

;;; * Window switching

(defparameter *window-names* nil
  "Text that should be displayed instead of the window class.")

(defun current-head? (win)
  (eq
   (stumpwm:current-head)
   (stumpwm:window-head win)))

;; TODO Keep a history of windows?
(defun get-windows ()
  (let ((groups (stumpwm:screen-groups (stumpwm:current-screen)))
        (current-win (stumpwm:current-window))
        (current-group (stumpwm:current-group))
        (windows nil))

    (multiple-value-bind (windows-in-current-head other-windows)
        (serapeum:partition #'current-head? (stumpwm:group-windows current-group))

      (loop :for win :in windows-in-current-head :do
        (pushnew win windows))

      (loop :for win :in other-windows :do
        (pushnew win windows)))

    (loop :for win :in (mapcan #'stumpwm:group-windows groups) :do
      (pushnew win windows))

    (remove-if (lambda (w) (eq w current-win))
               (nreverse windows))

    ;; (remove-duplicates
    ;;  (remove-if (lambda (w) (eq w current-win))
    ;;             (alexandria:flatten
    ;;              (concatenate
    ;;               'list
    ;;               (stumpwm:group-windows current-group)
    ;;               (mapcar #'stumpwm:group-windows groups))))
    ;;  :from-end t)
    ))

(defun format-window (win)
  (let ((class (stumpwm:window-class win)))
    (list
     (stumpwm:group-name (stumpwm:window-group win))
     (or (cdr (assoc class *window-names* :test #'string-equal)) (str:downcase class))
     (stumpwm:window-title win))))

(defun select-window ()
  (let* ((windows (get-windows))
         (idx (nth-value 1 (run (mapcar #'format-window windows) :prompt "window"))))
    (when idx
      (nth idx windows))))

(stumpwm:defcommand rofi-windows () ()
  "Jump to any window in any group, switching groups if necessary."
  (let ((window (select-window)))
    (when window
      (stumpwm::pull-window window)
      ;; (stumpwm::focus-all window)
      )))

(stumpwm:defcommand rofi-pull-global () ()
  "Pull window from any group to the current one."
  (let ((window (select-window)))
    (when window
      (stumpwm:move-window-to-group window (stumpwm:current-group))
      (stumpwm::focus-all window))))
