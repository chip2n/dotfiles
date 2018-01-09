(provide 'bolt-mode)

(setq bolt-highlights
      (let* (
             (x-keywords '("type" "extends" "path" "is"))
             (x-types '("String" "Boolean" "Number" "Null" "Map"))
             (x-functions '("read" "write" "validate" "create" "update" "delete" "index"))
             (x-constants '("now"))

             (x-keywords-regexp (regexp-opt x-keywords 'words))
             (x-types-regexp (regexp-opt x-types 'words))
             (x-functions-regexp (regexp-opt x-functions 'words))
             (x-user-functions-regexp "^\\([[:alnum:]]+\\)\(.*\) \{")
             (x-constants-regexp (regexp-opt x-constants 'words))
             (x-comments-regexp "^.*\\(//.*\\)")
             )
        `(
          (,x-comments-regexp . (1 font-lock-comment-face))
          (,x-keywords-regexp . font-lock-keyword-face)
          (,x-types-regexp . font-lock-type-face)
          (,x-functions-regexp . font-lock-function-name-face)
          (,x-user-functions-regexp . (1 font-lock-function-name-face))
          (,x-constants-regexp . font-lock-constant-face)
          )
        )
      )

(define-derived-mode bolt-mode fundamental-mode "bolt"
  "Major mode for editing Firebase Bolt rules"
  (setq font-lock-defaults '(bolt-highlights)))

(add-to-list 'auto-mode-alist '("\\.bolt$" . bolt-mode))
