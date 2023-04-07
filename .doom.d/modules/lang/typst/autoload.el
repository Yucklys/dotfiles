(defgroup typst nil
  "Typst Writing."
  :group 'text)

(defvar typst-global-keywords
  '("#let" "#set" "#show" "#if" "#for" "#while" "#include" "#import")
  "Keywords for typst mode that are in the global scope.")

;; (defun typst-inner-face-match (limit)
;;   (catch 'found
;;     (while (re-search-forward "{\\([^}]*\\)}" limit t)
;;       (let ((text-start (match-beginning 1))
;;              (text-end (match-end 1)))
;;         (when (and text-start text-end)
;;           (set-match-data (list text-start text-end))
;;           (throw 'found t))))
;;     nil))

;; (defface typst-custom-face
;;   '((t (:foreground "green")))
;;   "Custom face for text inside braces."
;;   :group 'markup-faces)

(defvar typst-font-lock-keywords
  `((,(regexp-opt typst-global-keywords t) . font-lock-keyword-face)
     ("\\(#\\w+\\)[[(]" . '(1 font-lock-function-name-face))
     ;; (typst-inner-face-match . typst-custom-face)
     )
  "Minimal highlighting expressions for typst mode")

;;;###autoload
(define-derived-mode typst-mode prog-mode "Typst"
  "A major mode for editing the markup-based typesetting language."
  ;; :syntax-table typst-syntax-table
  (setq font-lock-defaults '(typst-font-lock-keywords)))


;;;###autoload
(add-to-list 'auto-mode-alist '("\\.typ\\'" . typst-mode))

(provide 'typst-mode)
