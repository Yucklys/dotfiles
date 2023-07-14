;; using system ls command for dired
(setq insert-directory-program "/usr/bin/ls")
;; exec PATH
(setenv "PATH" (concat (getenv "PATH") ":/home/yucklys/.local/share/gem/ruby/3.0.0/bin"))
(setq exec-path (append exec-path '("/home/yucklys/.local/share/gem/ruby/3.0.0/bin")))

;; (use-package! circadian
;;   :config
;;   (setq circadian-themes '(("8:00" . doom-nord-light)
;;                            ("19:30" . doom-nord)))
;;   (circadian-setup))
(setq doom-theme 'doom-nord)
;; Fonts managed by cnfonts
(setq doom-font (font-spec :family "MonoLisa" :size 32))
(setq fancy-splash-image
      (let* ((banners (directory-files "~/.doom.d/banner" 'full (rx ".png" eos)))
             (banner (elt banners (random (length banners)))))
        banner))

;; (add-to-list 'default-frame-alist '(font . "FiraCode-12"))

(global-set-key (kbd "<f5>") 'revert-buffer)
(global-git-gutter-mode +1)

;; (setq url-proxy-services
;;       '(("no_proxy" . "^\\(localhost\\|10\\..*\\|192\\.168\\..*\\)")
;;         ("http" . "127.0.0.1:7890")
;;         ("https" . "127.0.0.1:7890")))

(require 'pinyinlib)
(defun completion--regex-pinyin (str)
  (orderless-regexp (pinyinlib-build-regexp-string str)))

(use-package! orderless
  :config
  (add-to-list 'orderless-matching-styles 'completion--regex-pinyin)
  )

;; Disable IME on Windows
;; (w32-set-ime-open-status nil)

;; Start server
(require 'org-protocol)
(use-package! org-roam-protocol :after org-protocol)

;; Start server by systemd
;; (server-start)

(defun meow-setup ()
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-dvorak)
  (meow-motion-overwrite-define-key
   '("j" . meow-next)
   '("k" . meow-prev)
   '("l" . meow-right)
   '("h" . meow-left))
  (meow-leader-define-key
   ;; SPC j/k will run the original command in MOTION state.
   '("j" . meow-motion-origin-command)
   '("k" . meow-motion-origin-command)
   '("l" . meow-motion-origin-command)
   '("h" . meow-motion-origin-command)
   ;; '("[" . centaur-tabs-backward)
   ;; '("]" . centaur-tabs-forward)
   ;; Use SPC (0-9) for digit arguments.
   '("1" . meow-digit-argument)
   '("2" . meow-digit-argument)
   '("3" . meow-digit-argument)
   '("4" . meow-digit-argument)
   '("5" . meow-digit-argument)
   '("6" . meow-digit-argument)
   '("7" . meow-digit-argument)
   '("8" . meow-digit-argument)
   '("9" . meow-digit-argument)
   '("0" . meow-digit-argument)
   '("/" . meow-keypad-describe-key)
   (cons "p" projectile-command-map)
   '("wh" . split-window-horizontally)
   '("wv" . split-window-vertically)
   '("SPC" . meow-cancel-selection)
   '("?" . meow-cheatsheet))
  (meow-normal-define-key
   '("%" . meow-query-replace-regexp)
   '("&" . meow-query-replace)
   '("," . meow-inner-of-thing)
   '("-" . negative-argument)
   '("." . meow-bounds-of-thing)
   '("/" . meow-visit)
   '("0" . meow-expand-0)
   '("1" . meow-expand-1)
   '("2" . meow-expand-2)
   '("3" . meow-expand-3)
   '("4" . meow-expand-4)
   '("5" . meow-expand-5)
   '("6" . meow-expand-6)
   '("7" . meow-expand-7)
   '("8" . meow-expand-8)
   '("9" . meow-expand-9)
   '(";" . meow-reverse)
   '("<" . meow-beginning-of-thing)
   '(">" . meow-end-of-thing)
   '("A" . meow-open-below)
   '("B" . meow-back-symbol)
   '("E" . meow-next-symbol)
   '("F" . meow-find-expand)
   '("G" . meow-grab)
   '("H" . meow-left-expand)
   '("I" . meow-open-above)
   '("J" . meow-next-expand)
   '("K" . meow-prev-expand)
   '("L" . meow-right-expand)
   '("M" . meow-mark-symbol)
   '("M-j" . meow-join)
   '("N" . flycheck-previous-error)
   '("O" . meow-to-block)
   '("P" . meow-yank-pop)
   '("Q" . meow-goto-line)
   '("R" . meow-swap-grab)
   '("S" . meow-pop-search)
   '("T" . meow-till-expand)
   '("U" . undo-redo)
   '("V" . meow-kmacro-matches)
   '("W" . meow-next-symbol)
   '("X" . meow-kmacro-lines)
   '("Y" . meow-sync-grab)
   '("Z" . meow-pop-all-selection)
   '("a" . meow-append)
   '("b" . meow-back-word)
   '("c" . meow-change)
   '("d" . meow-kill)
   '("e" . meow-next-word)
   '("f" . meow-find)
   '("gb" . meow-page-up)
   '("gd" . meow-page-down)
   '("ge" . end-of-buffer)
   '("gg" . beginning-of-buffer)
   '("gh" . beginning-of-line)
   '("gi" . beginning-of-line-text)
   '("gl" . end-of-line)
   '("gt" . meow-goto-line)
   '("h" . meow-left)
   '("i" . meow-insert)
   '("j" . meow-next)
   '("k" . meow-prev)
   '("l" . meow-right)
   '("m" . meow-mark-word)
   '("n" . flycheck-next-error)
   '("o" . meow-block)
   '("p" . meow-yank)
   '("q" . meow-quit)
   '("r" . meow-replace)
   '("s" . meow-search)
   '("t" . meow-till)
   '("u" . meow-undo)
   '("v" . recenter-top-bottom)
   '("w" . meow-next-word)
   '("x" . meow-line)
   '("y" . meow-save)
   '("z" . meow-pop-selection)))
(use-package meow
  :demand t
  :init
  (meow-global-mode 1)
  :config
  (setq meow-selection-command-fallback '((meow-change . meow-change-char)
                                          (meow-kill . meow-C-d)
                                          (meow-cancel-selection . keyboard-quit)
                                          (meow-pop-selection . meow-pop-grab)
                                          (meow-beacon-change . meow-beacon-change-char)))
  (meow-setup)
  (meow-setup-line-number)
  (meow-setup-indicator))

;; Org modern
(add-hook 'org-mode-hook #'org-modern-mode)
(add-hook 'org-agenda-finalize-hook #'org-modern-agenda)

(custom-set-faces
 '(org-level-1 ((t (:height 1.4))))
 '(org-level-2 ((t (:height 1.3))))
 '(org-level-3 ((t (:height 1.15))))
 )
(add-hook 'org-mode-hook 'org-indent-mode)
(setq org-hide-emphasis-markers nil)
(add-hook 'org-mode-hook (lambda () (auto-fill-mode 0)))

(after! org
  (setq org-hide-emphasis-markers t)
  (setq org-log-done t)
  (setq org-agenda-start-with-clockreport-mode t
        org-agenda-use-time-grid t)
  (setq org-priority-faces '((?A :foreground "#BF616A")
                             (?B :foreground "#ebcb8b")
                             (?C :foreground "#81A1C1")))
  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
          (sequence "REPORT(r)" "BUG(b)" "KNOWNCAUSE(k)" "|" "FIXED(f)")
          (sequence "|" "CANCEL(c)")))
  (setq org-todo-keyword-faces
        '(("TODO" :foreground "#b48ead" :weight normal :underline t) ("NEXT" :foreground "#88c0d0" :weight normal :underline t) ("DONE" :foreground "#a3be8c" :weight normal :underline t)
          ("ISSUE" :foreground "#bf616a" :weight normal :underline t) ("FIXED" :foreground "#a3be8c" :weight normal :underline t)
          ("CANCEL" :foreground "#bf616a" :underline t)))
  (setq org-image-actual-width '(400)))

;;;; Make verbatim with highlight text background.
;;(add-to-list 'org-emphasis-alist
;;           '("=" (:background "#EBCB8B"
;;                  :foreground "#4C566A")))
;;;; Make deletion(obsolote) text foreground with dark gray.
;;(add-to-list 'org-emphasis-alist
;;           '("+" (:foreground "dark gray"
;;                  :strike-through t)))
;;;; Make code style around with box.
;;(add-to-list 'org-emphasis-alist
;;           '("~" (:box (:line-width 1
;;                        :color "grey75"
;;                        :style released-button))))

(advice-add #'outline-hide-subtree :after #'dwim-unfontify-last-line-of-subtree)
(advice-add #'outline-show-heading :after #'dwim-fontify-last-line-of-block)

;; Org mode custom
(custom-set-variables
 '(org-directory "~/OneDrive/org/")
 '(org-default-notes-file (concat org-directory "/notes.org"))
 '(org-export-html-postamble nil)
 '(org-hide-leading-stars t)
 '(org-startup-folded (quote overview))
 '(org-startup-indented t)
 )

(setq org-agenda-files (list "~/OneDrive/org/i.org"
                             "~/OneDrive/org/todo.org"
                             "~/OneDrive/org/roam/50 Journals/"))

;; Auto refile
(add-hook 'org-after-todo-state-change-hook 'dk/refile-todo 'append)
(defun dk/refile-todo()
  (if (equal org-state "DONE")
      (dk/refile-to "~/OneDrive/org/todo.org" "Done"))
  (if (equal org-state "CANCEL")
      (dk/refile-to "~/OneDrive/org/todo.org" "Cancel"))
  (if (equal org-state "NEXT")
      (dk/refile-to "~/OneDrive/org/todo.org" "Next")))

(defun dk/refile-to (file headline)
  "Move current headline to specified location"
  (let ((pos (save-excursion
               (find-file file)
               (org-find-exact-headline-in-buffer headline))))
    (org-refile nil nil (list headline file nil pos)))
  (switch-to-buffer (current-buffer))
  )

;; Org capture templates
(with-eval-after-load 'org-capture
  (defun org-hugo-new-subtree-post-capture-template ()
    "Returns `org-capture' template string for new Hugo post.
See `org-capture-templates' for more information."
    (let* ((title (read-from-minibuffer "Post Title: ")) ;Prompt to enter the post title
           (date (format-time-string (org-time-stamp-format "%Y-%m-%d") (org-current-time)))
           (fname (org-hugo-slug title)))
      (mapconcat #'identity
                 `(
                   ,(concat "* TODO " title)
                   ":PROPERTIES:"
                   ":EXPORT_HUGO_CUSTOM_FRONT_MATTER: :categories '()"
                   ":EXPORT_HUGO_CUSTOM_FRONT_MATTER: :tags '()"
                   ":EXPORT_HUGO_CUStOM_FRONT_MATTER: :toc true"
                   ":EXPORT_HUGO_CUStOM_FRONT_MATTER: :comment true"
                   ,(concat ":EXPORT_DATE: " date)
                   ,(concat ":EXPORT_FILE_NAME: " fname)
                   ":END:"
                   "%?\n")          ;Place the cursor here finally
                 "\n"))))

(defun transform-square-brackets-to-round-ones(string-to-transform)
  "Transforms [ into ( and ] into ), other chars left unchanged."
  (concat
   (mapcar #'(lambda (c) (if (equal c ?\[) ?\( (if (equal c ?\]) ?\) c))) string-to-transform))
  )

(defadvice org-capture
    (after make-full-window-frame activate)
  "Advise capture to be the only window when used as a popup"
  (if (equal "emacs-capture" (frame-parameter nil 'name))
      (switch-to-buffer (doom-fallback-buffer))
    (delete-other-windows)))

(defadvice org-capture-finalize
    (after delete-capture-frame activate)
  "Advise capture-finalize to close the frame"
  (if (equal "emacs-capture" (frame-parameter nil 'name))
      (delete-frame)))

(use-package! org-capture
  :config
  (setq org-capture-templates '(
                                ("t" "Todo" entry (file+headline "todo.org" "Task")
                                 "** TODO %?\n")
                                ("b" "Blog" entry (file+olp "blog.org" "Blog posts")
                                 (function org-hugo-new-subtree-post-capture-template))
                                )))

;; Log when mark as DONE
(setq org-log-done 'time)

(setq org-reveal-root "https://revealjs.com")

(use-package! websocket
  :after org-roam)

(use-package! org-roam-ui
  :after org-roam ;; or :after org
  ;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
  ;;         a hookable mode anymore, you're advised to pick something yourself
  ;;         if you don't care about startup time, use
  ;;  :hook (after-init . org-roam-ui-mode)
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t)
  (map! :leader
        :desc "Open Org Roam UI"
        "n r g" #'org-roam-ui-mode)
  )

(use-package! org-roam
  :init
  (require 'org-roam-protocol)
  :config
  (setq org-roam-directory "~/OneDrive/org/roam")
  (setq org-roam-completion-everywhere t)
  (setq org-roam-dailies-directory "50 Journals")
  (setq org-roam-dailies-capture-templates
        '(("d" "default" entry "* %<%H:%M %p>\n%?"
           :if-new (file+datetree "%<%Y-%m>.org"
                                  'day)
        )))
  (setq org-roam-capture-templates '(
                                     ("n" "Note Group")
                                     ("np" "Paper Note" plain
                                      "* Related Work\n%?\n* Argument\n\n* Experiment\n\n* Conclusion"
                                      :if-new (file+head "10 Areas/12 ${slug}.org" "#+title: ${title}\n")
                                      :unnarrowed t)
                                     ("nr" "Reading" entry
                                      "** %?"
                                      :if-new (file+head+olp "10 Areas/11 ${slug}.org" "#+title: ${title}\n* Intro\n\n" ("Highlights"))
                                      :unnarrowed t)
                                     ("c" "Card Group")
                                     ("cc" "Concept" plain
                                      "* Source\n\n%?\n\n"
                                      :if-new (file+head "00 Cards/01 ${slug}.org" "#+title: ${title}\n")
                                      :unnarrowed t)
                                     ("cg" "Game" plain
                                      "* Info\n\n%?\n\n* Commit"
                                      :if-new (file+head "00 Cards/02 ${slug}.org" "#+title: ${title}\n")
                                      :unnarrowed t)
                                     ("ct" "Topic" plain
                                      "%?"
                                      :if-new (file+head "00 Cards/00 ${slug}.org" "#+title: ${title}\n")
                                      :unnarrowed t)
                                     ("p" "Project" plain
                                      "%?"
                                      :if-new (file+head "20 Projects/20 ${slug}.org" "#+title: ${title}\n#+filetag:\n")
                                      :unnarrowed t)
                                     ))
  (setq org-roam-capture-ref-templates '(
                                         ("r" "ref" plain "* Summary\n%?" :if-new
                                          (file+head "00 Cards/03 ${slug}.org" "#+title: ${title}")
                                          :unnarrowed t)
                                         ))
 )

(require 'org-download)

;; Drag-and-drop to `dired`
(add-hook 'dired-mode-hook 'org-download-enable)

;; set org download directory
(setq org-download-method 'attach)

(use-package! ox-hugo
  :config
  (setq org-hugo-date-format "%Y-%m-%d")
  (setq org-hugo-suppress-lastmod-period 86400.0)
  (setq org-hugo-auto-set-lastmod t))

;;functions to support syncing .elfeed between machines
;;makes sure elfeed reads index from disk before launching
(defun bjm/elfeed-load-db-and-open ()
  "Wrapper to load the elfeed db from disk before opening"
  (interactive)
  (elfeed-db-load)
  (elfeed)
  (elfeed-search-update--force))

;;write to disk when quiting
(defun bjm/elfeed-save-db-and-bury ()
  "Wrapper to save the elfeed db to disk before burying buffer"
  (interactive)
  (elfeed-db-save)
  (quit-window))

(defun elfeed-mark-all-as-read ()
  (interactive)
  (mark-whole-buffer)
  (elfeed-search-untag-all-unread))

(use-package! elfeed
  :bind (:map elfeed-search-mode-map
          ("q" . bjm/elfeed-save-db-and-bury)
          ("Q" . bjm/elfeed-save-db-and-bury)
          )
  )

(use-package! elfeed-org
  :config
  (elfeed-org)
  (setq rmh-elfeed-org-files (list "~/OneDrive/org/elfeed.org")))

(use-package! company
  :config
  (setq company-show-numbers t)
  (setq company-tooltip-align-annotations t)
  (setq company-idle-delay 0)
  (add-to-list 'company-backends #'company-tabnine)
  )

(after! lsp-mode
  (setq +lsp-company-backends '(company-capf company-yasnippet
                                              :separate company-tabnine
                                              )))

;; (use-package! eglot-x
;;   :load-path "~/.doom.d/snippets")

;; (after! rustic
;;   (set-company-backend! 'rustic-mode '(company-capf company-files company-yasnippet
;;                                        ;; :with company-tabnine
;;                                        )))


(use-package! rainbow-delimiters
  :hook
  (prog-mode . rainbow-delimiters-mode))

(setq projectile-indexing-method 'native)

(use-package! tree-sitter
  :config
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(after! rustic
  (setq rustic-lsp-server 'rust-analyzer)
  (setq lsp-rust-analyzer-server-display-inlay-hints t))

;; (use-package! lsp-mode
;;   :config
;;   (setq lsp-rust-analyzer-proc-macro-enable t))

(add-to-list 'load-path "/home/yucklys/.opam/default/share/emacs/site-lisp")
(require 'ocp-indent)

;; (defhydra hydra-elfeed ()
;;   "
;;                                -- ELFEED MENU --
;;
;; "
;;   ("O" (find-file "~/OneDrive/org/elfeed.org") "Edit source list" :color blue :column "EDIT")
;;   ("u" elfeed-update "Update")
;;   ("e" (elfeed-search-set-filter "@6-months-ago +unread +emacs") "emacs" :column "QUERY")
;;   ("b" (elfeed-search-set-filter "@6-months-ago +unread +blog") "blog")
;;   ("n" (elfeed-search-set-filter "@6-months-ago +unread +news") "news")
;;   ("c" (elfeed-search-set-filter "@6-months-ago +unread +creative") "creative")
;;   ("f" (elfeed-search-set-filter "@6-months-ago +unread +fun") "fun")
;;   ("t" (elfeed-search-set-filter "@6-months-ago +unread +tech") "tech")
;;   ("p" (elfeed-search-set-filter "@6-months-ago +unread +programming") "programming")
;;   ("l" (elfeed-search-set-filter "@6-months-ago +unread +linux") "linux")
;;   ("a" (elfeed-search-set-filter "@6-months-ago") "all")
;;   ("T" (elfeed-search-set-filter "@1-day-ago") "today")
;;   ("q" nil "quit" :color blue :column "QUIT"))

;; (defhydra hydra-mingus ()
;;   "
;;                                -- MINGUS MENU --
;;
;;
;;   ("r" (mingus-random) "[R]andom" :color blue :column "PLAYMETHOD")
;;   ("s" (mingus-single) "[S]ingle" :color blue)
;;   ("p" (mingus-repeat) "Re[p]eat" :color blue)
;;   ("C" (mingus-clear) "[C]lear playlist" :exit t :column "PLAYLIST")
;;   ("S" (mingus-save-playlist) "[S]ave playlist" :exit t)
;;   ("l" (mingus-load-playlist) "[L]oad playlist" :exit t)
;;   ("U" (mingus-update) "[U]pdate" :exit t)
;;   ("q" nil "[Q]uit hydra" :exit t :column "QUIT")
;;   ("Q" (mingus-git-out) "[Q]uit mingus" :exit t))

;; (pdf-tools-install)
(defun nolinum ()
  (global-linum-mode 0)
  )

(defun dwim-unfontify-last-line-of-subtree (&rest _)
  "Unfontify last line of subtree if it's a source block."
  (save-excursion
    (org-end-of-subtree)
    (beginning-of-line)
    (when (looking-at-p (rx "#+end_src"))
      (font-lock-unfontify-region
       (line-end-position) (1+ (line-end-position))))))

(defun dwim-fontify-last-line-of-block (&rest _)
  "Do what I mean: fontify last line of source block.
    When the heading has a source block as the last item (in the subtree) do the
      following:
    If the source block is now visible, fontify the end its last line.
    If it’s still invisible, unfontify its last line."
  (let (font-lock-fn point)
    (save-excursion
      (org-end-of-subtree)
      (beginning-of-line)
      (run-hooks 'outline-view-change-hook)
      (when (looking-at-p (rx "#+end_src"))
        (setq font-lock-fn
              (if (invisible-p (line-end-position))
                  #'font-lock-unfontify-region
                #'font-lock-fontify-region))
        (funcall font-lock-fn
                 (line-end-position)
                 (1+ (line-end-position)))))))

(use-package! mu4e
  :config
  (setq mu4e-get-mail-command "mbsync -a")
  (setq mu4e-update-interval 60)
  (setq user-mail-address "yucklys687@gmail.com")
  (setq +mu4e-gmail-accounts '(("yucklys687@gmail.com" . "/work")
                               ("zli12330@terpmail.umd.edu" . "/umd")))
  (set-email-account! "work"
                      '((mu4e-sent-folder    . "/work/Sent Mail")
                        (mu4e-drafts-folder  . "/work/Drafts")
                        (mu4e-trash-folder   . "/work/Trash")
                        (smtpmail-smtp-user  . "yucklys687@gmail.com")))
  (set-email-account! "personal"
                      '((mu4e-sent-folder    . "/personal/Sent Mail")
                        (mu4e-drafts-folder  . "/personal/Drafts")
                        (mu4e-trash-folder   . "/personal/Trash")
                        (smtpmail-smtp-user  . "yucklys687@outlook.com")))
    (set-email-account! "umd"
                      '((mu4e-sent-folder    . "/umd/Sent Mail")
                        (mu4e-drafts-folder  . "/umd/Drafts")
                        (mu4e-trash-folder   . "/umd/Trash")
                        (smtpmail-smtp-user  . "zli12330@terpmail.umd.edu"))))

(use-package! deft
  :config
  (setq deft-extensions '("org"))
  (setq deft-directory "~/OneDrive/org")
  (setq deft-recursive t)
  (setq deft-strip-summary-regexp
        (concat "\\("
                "[\n\t]" ;; blank
                "\\|^#\\+[[:upper:]_]+:.*$" ;; org-mode metadata
                "\\|^#\\+[[:alnum:]_]+:.*$" ;; org-mode metadata
                "\\)"))
  (setq deft-file-naming-rules '((noslash . "_")))
  (setq deft-text-mode 'org-mode)
  (setq deft-use-filter-string-for-filename t)
  (setq deft-org-mode-title-prefix t)
  (setq deft-use-filename-as-title nil))

(defun rime-predicate-meow-mode-p ()
  "Detect whether the current buffer is in `meow' state.

Include `meow-normal-state' ,`meow-motion-state' ,
`meow-keypad-state'.

Can be used in `rime-disable-predicates' and `rime-inline-predicates'."
  (and (fboundp 'meow-mode)
       (or (meow-normal-mode-p)
           (meow-keypad-mode-p)
           (meow-motion-mode-p))))
;; use emacs-rime
(use-package! rime
  :custom
  (default-input-method "rime")
  :config
  (setq rime-disable-predicates
        '(rime-predicate-meow-mode-p
          rime-predicate-after-alphabet-char-p
          rime-predicate-tex-math-or-command-p
          rime-predicate-punctuation-after-space-cc-p
          rime-predicate-prog-in-code-p
          ;; rime-predicate-punctuation-line-begin-p
          ;; rime-predicate-current-uppercase-letter-p
          ))
  (setq rime-inline-predicates
        '(rime-predicate-space-after-cc-p))
  (setq rime-inline-ascii-trigger 'shift-l)
  (setq rime-show-candidate 'minibuffer)
  (define-key rime-mode-map (kbd "M-i") 'rime-force-enable))

;; enable pangu-spacing
(use-package! pangu-spacing
  :config
  (global-pangu-spacing-mode 1))

;; (use-package! pyim
;;   :config
;;   (setq default-input-method "pyim"))

;; use fcitx
;; (use-package! fcitx
;;   :config
;;   (setq fcitx-remote-command "fcitx5-remote"))

;; Auto generated by cnfonts
;; <https://github.com/tumashu/cnfonts>
;; (set-face-attribute
;;  'default nil
;;  :font (font-spec :name "-outline-Iosevka-bold-normal-normal-mono-*-*-*-*-c-*-iso10646-1"
;;                   :weight 'normal
;;                   :slant 'normal
;;                   :size 12.5))
;; (dolist (charset '(kana han symbol cjk-misc bopomofo))
;;   (set-fontset-font
;;    (frame-parameter nil 'font)
;;    charset
;;    (font-spec :name "文泉驿等宽微米黑"
;;               :weight 'normal
;;               :slant 'normal
;;               :size 12.5)))

(setq ispell-dictionary "en")

;; (use-package! nyan-mode
;;   :config
;;   (setq nyan-animate-nyancat t)
;;   (setq nyan-wavy-trail t))
;;
;; (add-hook! 'prog-mode-hook 'nyan-mode)

;; (use-package! eaf
;;   :load-path "~/.emacs.d/site-lisp/emacs-application-framework"
;;   :config
;;   (setq eaf-proxy-type "http")
;;   (setq eaf-proxy-host "127.0.0.1")
;;   (setq eaf-proxy-port "7890")
;;   (setq eaf-python-command "python"))
;;
;; (use-package! eaf-browser)
;;
;; (use-package! eaf-demo)

(load-file "~/.doom.d/keymap.el")
