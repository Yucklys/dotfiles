;;; ~/.doom.d/keymap.el -*- lexical-binding: t; -*-

(setq doom-localleader-alt-key "C-l")
(map! :map meow-leader-keymap
  "l" #'meow-keypad-start)

(map! :map doom-leader-workspaces/windows-map
    "v"       #'+hydra/window-nav/body
     "-"       #'split-window-vertically
     "\\"      #'split-window-horizontally)

;; (define-key company-active-map (kbd "<return>") #'company-complete-selection)
(add-to-list 'meow-keymap-alist (cons 'leader doom-leader-map))

;; (setq evil-escape-key-sequence "jj")

(global-unset-key "\C-h")
