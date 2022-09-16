;;; ~/.doom.d/keymap.el -*- lexical-binding: t; -*-

(map! :map doom-leader-workspaces/windows-map
      "v"       #'+hydra/window-nav/body
      "-"       #'split-window-vertically
      "\\"      #'split-window-horizontally)

;; (define-key company-active-map (kbd "<return>") #'company-complete-selection)
(add-to-list 'meow-keymap-alist (cons 'leader doom-leader-map))


(global-unset-key "\C-h")
