;;; ~/.doom.d/keymap.el -*- lexical-binding: t; -*-

(map!
 (:map pyim-mode-map
   "]"   #'pyim-page-next-page
   "["   #'pyim-page-previous-page
   ";"   (lambda () (interactive) (pyim-page-select-word-by-number 2)))
 (:map evil-normal-state-map
   "C-e" #'evil-end-of-line
   "C-a" #'beginning-of-line-text)
 (:map evil-visual-state-map
   "C-e" #'evil-end-of-line
   "C-a" #'beginning-of-line-text)
 (:map mingus-playlist-map
   "RET" #'mingus-play
   "s"   #'mingus-stop
   "SPC" #'mingus-toggle
   "q"   #'mingus-git-out
   "j"   #'next-line
   "k"   #'previous-line
   "?"   #'mingus-help
   "-"   #'mingus-vol-down
   "+"   #'mingus-vol-up
   "d"   #'mingus-del
   "."   #'hydra-mingus/body)
 (:map elfeed-show-mode-map
   "\["   #'elfeed-show-prev
   "]"    #'elfeed-show-next)
 :leader
 ("DEL"   #'org-mark-ring-goto)
 (:map evil-normal-state-map
   "[" #'centaur-tabs-backward
   "]" #'centaur-tabs-forward)
 (:prefix-map ("c" . "code")
   :desc "Format buffer/region (format-all)"        "f"   #'format-all-buffer
   :desc "LSP imenu"                                "i"   #'lsp-ui-imenu
   :desc "Treemacs iagnostic list"                  "e"   #'lsp-treemacs-errors-list
   (:prefix-map ("/" . "search")
     :desc "Goto char"                         "c"   #'avy-goto-char
     :desc "Search buffer"                     "/"   #'swiper)
   (:prefix-map ("n" . "notes")
     :desc "New journal"                       "j"   #'org-journal-new-entry))
 (:prefix-map ("o" . "open")
   :desc "mingus"                              "m"   #'mingus
   :desc "elfeed"                              "e"   #'elfeed
   :desc "+eshell/toggle"                      "t"   #'+eshell/toggle))

(global-set-key "\M-s" 'avy-goto-char)
(global-set-key "\M-i" 'pyim-convert-string-at-point)
(global-set-key (kbd "<f8>") 'treemacs)
(global-unset-key "\C-h")
