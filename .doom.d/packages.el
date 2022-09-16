;; -*- no-byte-compile: t; -*-
;;; ~/.doom.d/packages.el

;;; Examples:
;; (package! some-package)
;; (package! another-package :recipe (:fetcher github :repo "username/repo"))
;; (package! builtin-package :disable t)

;; lsp-mode
(package! lsp-mode :pin "6327359f3b5e19aeaa1c9ee6bd9b80b51f95f843")

;; (package! org-noter)
;; (package! org-journal)
;; (package! mingus)
;; (package! circadian)
(package! telega)
(package! cnfonts)
(package! ox-hugo :disable t)
;; (package! exwm)
;; (package! wucuo)
;;(package! org-fancy-priorities)
(package! rime)
(package! pinyinlib)
(package! pyim :disable t)
(package! tree-sitter)
(package! tree-sitter-langs)
(package! company-tabnine)

;; Org
;; Org custom
(package! org-modern)
;; Org roam
(package! websocket)
(package! org-roam-ui :recipe (:host github :repo "org-roam/org-roam-ui" :files ("*.el" "out")))
;; Org image
(package! org-download)
;; Editor
(package! meow)
