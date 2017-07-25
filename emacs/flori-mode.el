
(setq flori-keywords '("ns" "c-ffi" "match" "if" "cond" "for" "while"))
(setq flori-annot-keywords '())

(setq flori-def-regexp "\\(def.+?\\)\s+\\(.+?\\)\\(\s\\|\n\\)")
(setq flori-keywords-regexp (regexp-opt flori-keywords 'words))
(setq flori-funcname-regexp "\\(def.+\\)?\s+\\(.+?\\)]\s")
(setq flori-type-regexp "[A-Z]\\([a-z]\\|[0-9]\\)*")
(setq flori-annot-keywords-regexp (regexp-opt flori-annot-keywords 'words))
(setq flori-attr-regexp "\:\\([a-z]\\|[A-Z]\\)+")
(setq flori-constant-regexp "[0-9]\\([0-9]\\|\.\\)*")
(setq flori-string-regexp "\".*\"")
(setq flori-path-regexp "\\([a-z]\\|[A-Z]\\)+\\(\\.\\([a-z]\\|[A-Z]\\)+\\)+")
(setq flori-comment-regexp ";.*\n")

(setq flori-font-lock-keywords
      `((,flori-keywords-regexp . font-lock-keyword-face)
        (,flori-def-regexp (1 font-lock-keyword-face) (2 font-lock-function-name-face))
        (,flori-funcname-regexp . font-lock-function-name-face)
        (,flori-type-regexp . font-lock-type-face)
        (,flori-annot-keywords-regexp . font-lock-builtin-face)
        (,flori-attr-regexp . font-lock-preprocessor-face)
        (,flori-constant-regexp . font-lock-constant-face)
        (,flori-string-regexp . font-lock-string-face)
        (,flori-path-regexp . font-lock-preprocessor-face)
        (,flori-comment-regexp . font-lock-comment-face)))

(defun flori-indent-line ()
  (save-excursion
    (beginning-of-line)
    (indent-line-to (* 2 (car (syntax-ppss))))))
(defun flori-indent-region (beg end)
  (indent-region beg end nil))

(define-derived-mode flori-mode fundamental-mode "Flori"
  (setq indent-tabs-mode nil)
  (setq indent-line-function #'flori-indent-line)
  (setq comment-start ";;")
  (setq font-lock-defaults '(flori-font-lock-keywords)))
(font-lock-add-keywords 'flori-mode
                        '(("@" . font-lock-builtin-face)))
(font-lock-add-keywords 'flori-mode
                        '((":\s" . font-lock-type-face)))
(font-lock-add-keywords 'flori-mode
                        '(("\\^\s" . font-lock-type-face)))

(provide 'flori-mode)
(add-to-list 'auto-mode-alist '("\\.flori$" . flori-mode))
