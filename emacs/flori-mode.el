
(setq flori-separates "\\((\\|)\\|[\\|]\\|\s\\|\n\\|\r\\)")

(setq flori-keywords '("construct" "match" "if" "cond" "for" "while" "require"))

(setq flori-keywords-regexp (regexp-opt flori-keywords 'words))
(setq flori-def-regexp (concat "\\(def.*?\\)" flori-separates "\\(.+?\\)" flori-separates))
(setq flori-type-regexp (concat "\\^.+?" flori-separates))
(setq flori-attr-regexp (concat "\:.+?" flori-separates))
(setq flori-constant-regexp (concat flori-separates "\\([0-9]\\([0-9]\\|\.\\)*?\\)" flori-separates))
(setq flori-string-regexp "\".*\"")
(setq flori-path-regexp "\\([a-z]\\|[A-Z]\\)+\\(\\.\\([a-z]\\|[A-Z]\\)+\\)+")

(setq flori-font-lock-keywords
      `((,flori-keywords-regexp . font-lock-keyword-face)
        (,flori-def-regexp (1 font-lock-keyword-face) (3 font-lock-function-name-face))
        (,flori-type-regexp . font-lock-type-face)
        (,flori-attr-regexp . font-lock-preprocessor-face)
        (,flori-constant-regexp (2 font-lock-constant-face))
        (,flori-string-regexp . font-lock-string-face)
        (,flori-path-regexp . font-lock-preprocessor-face)))

(defun flori-indent-line ()
  (save-excursion
    (beginning-of-line)
    (indent-line-to (* 2 (car (syntax-ppss)))))
  (forward-char (* 2 (car (syntax-ppss)))))
(defun flori-indent-region (beg end)
  (indent-region beg end nil))

(define-derived-mode flori-mode fundamental-mode "Flori"
  :syntax-table emacs-lisp-mode-syntax-table
  (setq indent-tabs-mode nil)
  (setq indent-line-function #'flori-indent-line)
  (setq comment-start ";;")
  (setq font-lock-defaults '(flori-font-lock-keywords)))
(font-lock-add-keywords 'flori-mode
                        '(("set!" . font-lock-keyword-face)))
(font-lock-add-keywords 'flori-mode
                        '(("destructor" . font-lock-builtin-face)))

(provide 'flori-mode)
(add-to-list 'auto-mode-alist '("\\.flori$" . flori-mode))
