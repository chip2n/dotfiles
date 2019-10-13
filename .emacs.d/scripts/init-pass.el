(use-package ivy-pass
  :ensure t)

(defmacro with-pass (args &rest body)
  (declare (indent 1))
  (let ((name (car args))
        (key (cadr args)))
    `(let ((,name (password-store-get ,key)))
       ,@body)))

(provide 'init-pass)
