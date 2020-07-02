(setq calendar-location-name "Beijing, CN")
(setq calendar-latitude 39.92)
(setq calendar-longitude 116.46)
;;(use-package! circadian
;;  :config
;;  (setq circadian-themes '(("8:00" . doom-nord-light)
;;                           ("19:30" . doom-nord)))
;;  (circadian-setup))
(setq doom-theme 'doom-nord)
(setq fancy-splash-image
      (let* ((banners (directory-files "~/.doom.d/banner" 'full (rx ".png" eos)))
             (banner (elt banners (random (length banners)))))
        banner))

(add-to-list 'default-frame-alist '(font . "Iosevka-12"))

(global-set-key (kbd "<f5>") 'revert-buffer)
(global-git-gutter-mode +1)

(add-hook 'org-mode-hook 'org-indent-mode)
(setq org-hide-emphasis-markers nil)
(add-hook 'org-mode-hook (lambda () (display-line-numbers-mode 0)))
(add-hook 'org-mode-hook (lambda () (auto-fill-mode 0)))

(+global-word-wrap-mode +1)


(after! org
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


(use-package! org-fancy-priorities
  :hook (org-mode . org-fancy-priorities-mode)
  :config
  (setq org-fancy-priorities-list '("■" "■" "■")))

(advice-add #'outline-hide-subtree :after #'dwim-unfontify-last-line-of-subtree)
(advice-add #'outline-show-heading :after #'dwim-fontify-last-line-of-block)

;; Org mode custom
(custom-set-variables
 '(org-directory "~/Notes/")
 '(org-default-notes-file (concat org-directory "/notes.org"))
 '(org-export-html-postamble nil)
 '(org-hide-leading-stars t)
 '(org-startup-folded (quote overview))
 '(org-startup-indented t)
 )

(setq org-agenda-files (list "~/Notes/i.org"
                             "~/Notes/todo.org"))

;; Auto refile
(add-hook 'org-after-todo-state-change-hook 'dk/refile-todo 'append)
(defun dk/refile-todo()
  (if (equal org-state "DONE")
      (dk/refile-to "~/Notes/todo.org" "Done"))
  (if (equal org-state "CANCEL")
      (dk/refile-to "~/Notes/todo.org" "Cancel"))
  (if (equal org-state "NEXT")
      (dk/refile-to "~/Notes/todo.org" "Next")))

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
   (mapcar #'(lambda (c) (if (equal c ?[) ?\( (if (equal c ?]) ?\) c))) string-to-transform))
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
                                ("d" "Diary" entry (file+olp+datetree "diary.org")
                                 "* %?\n")
                                ("i" "Idea" entry (file+headline "i.org" "Idea")
                                 "** %?\n%U\n")
                                ("p" "Protocol" entry (file+headline "i.org" "Inbox")
                                 "* %^{Title}\nSource: %u, %c\n #+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n\n%?")
                                ("L" "Protocol Link" entry (file+headline "i.org" "Inbox")
                                 "* %?\n:PROPERTIES:\n:CREATED: %U\n:END: \%i\n %a")
                                ("r" "Reading notes" entry (file+headline "books.org" "未分类")
                                 "* %U\n** 摘录\n#+BEGIN_QUOTE\n%?\n#+END_QUOTE\n** 笔记\n")
                                ("b" "Blog" entry (file+olp "blog.org" "Blog posts")
                                 (function org-hugo-new-subtree-post-capture-template)))))

;; Log when mark as DONE
(setq org-log-done 'time)

;; Org journal
(defun org-journal-date-format-func (time)
  "Custom function to insert journal date header,
and some custom text on a newly created journal file."
  (when (= (buffer-size) 0)
    (insert
     (pcase org-journal-file-type
       (`daily "#+TITLE: Daily Journal")
       (`weekly "#+TITLE: Weekly Journal")
       (`monthly "#+TITLE: Monthly Journal")
       (`yearly "#+TITLE: Yearly Journal"))))
  (concat org-journal-date-prefix (format-time-string "%A, %x" time)))


(use-package! org-journal
  :defer t
  :custom
  (org-journal-dir "~/Notes/journal/")
  (org-journal-date-format 'org-journal-date-format-func)
  (org-journal-file-type 'monthly)
  (org-journal-date-format "%A, %d %B %Y")
  (org-journal-enable-agenda-integration t))

(setq org-reveal-root "https://revealjs.com")

(use-package! org-roam
  :hook
  (after-init . org-roam-mode)
  :custom
  (org-roam-directory "~/Notes")
  :bind (:map org-roam-mode-map
              (("C-c n l" . org-roam)
               ("C-c n f" . org-roam-find-file)
               ("C-c n g" . org-roam-graph-show))
              :map org-mode-map
              (("C-c n i" . org-roam-insert))
              (("C-c n I" . org-roam-insert-immediate))))

(use-package! ox-hugo
  :config
  (setq org-hugo-date-format "%Y-%m-%d")
  (setq org-hugo-suppress-lastmod-period 86400.0)
  (setq org-hugo-auto-set-lastmod t))

;; (add-to-list 'load-path "~/Application/snails")
;; (require 'snails)
;; (add-hook 'snails-mode-hook #'(lambda () (evil-emacs-state)))

;; (add-to-list 'load-path "~/Application/fuz.el")
;; (add-to-list 'load-path "~/Application/fuz.el/target/release")
;; (require 'fuz)

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
  (setq rmh-elfeed-org-files (list "~/Notes/elfeed.org")))

(use-package! company
  :config
  (setq company-show-numbers t)
  (setq company-tooltip-align-annotations t))

(use-package! rainbow-delimiters
  :hook
  (prog-mode . rainbow-delimiters-mode))

(setq projectile-indexing-method 'native)

(use-package! lsp-mode
  :init
  (setq lsp-rust-server 'rust-analyzer)
  :config
  (setq lsp-rust-analyzer-server-display-inlay-hints t))
(use-package! rustic
  :config
  (setq rustic-lsp-server 'rust-analyzer))

(defhydra hydra-elfeed ()
  "
                               -- ELFEED MENU --

"
  ("O" (find-file "~/Notes/elfeed.org") "Edit source list" :color blue :column "EDIT")
  ("u" elfeed-update "Update")
  ("e" (elfeed-search-set-filter "@6-months-ago +unread +emacs") "emacs" :column "QUERY")
  ("b" (elfeed-search-set-filter "@6-months-ago +unread +blog") "blog")
  ("n" (elfeed-search-set-filter "@6-months-ago +unread +news") "news")
  ("c" (elfeed-search-set-filter "@6-months-ago +unread +creative") "creative")
  ("f" (elfeed-search-set-filter "@6-months-ago +unread +fun") "fun")
  ("t" (elfeed-search-set-filter "@6-months-ago +unread +tech") "tech")
  ("p" (elfeed-search-set-filter "@6-months-ago +unread +programming") "programming")
  ("l" (elfeed-search-set-filter "@6-months-ago +unread +linux") "linux")
  ("a" (elfeed-search-set-filter "@6-months-ago") "all")
  ("T" (elfeed-search-set-filter "@1-day-ago") "today")
  ("q" nil "quit" :color blue :column "QUIT"))

(defhydra hydra-mingus ()
  "
                               -- MINGUS MENU --

"
  ("r" (mingus-random) "[R]andom" :color blue :column "PLAYMETHOD")
  ("s" (mingus-single) "[S]ingle" :color blue)
  ("p" (mingus-repeat) "Re[p]eat" :color blue)
  ("C" (mingus-clear) "[C]lear playlist" :exit t :column "PLAYLIST")
  ("S" (mingus-save-playlist) "[S]ave playlist" :exit t)
  ("l" (mingus-load-playlist) "[L]oad playlist" :exit t)
  ("U" (mingus-update) "[U]pdate" :exit t)
  ("q" nil "[Q]uit hydra" :exit t :column "QUIT")
  ("Q" (mingus-git-out) "[Q]uit mingus" :exit t))

(pdf-tools-install)
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

(add-to-list 'load-path "~/Applications/emacs-application-framework")
(use-package! eaf
  :custom
  (eaf-find-alternate-file-in-dired t))
(require 'eaf-evil)

;; (require 'exwm)
;; (require 'exwm-config)
;; (exwm-config-default)
;; (require 'exwm-randr)
;; (setq exwm-randr-workspace-output-plist '(0 "eDP-1"))
;; (exwm-randr-enable)
;; (require 'exwm-systemtray)
;; (exwm-systemtray-enable)

(setq +mu4e-backend 'offlineimap)
(setq message-send-mail-function 'smtpmail-send-it)
(set-email-account! "yucklys687@outlook.com"
                    '((mu4e-sent-folder . "/Sent")
                      (mu4e-refile-folder . "/Inbox")
                      (mu4e-drafts-folder . "/Drafts")
                      (mu4e-trash-folder . "/Deleted")
                      (smtpmail-smtp-user . "yucklys687@outlook.com")
                      (mu4e-compose-signature . "---\nZekun Li"))
                    t)

(use-package! mingus
  :hook
  (mingus-playlist-mode . (lambda () (turn-off-evil-mode))))
(after! mingus
  (evil-make-overriding-map mingus-playlist-map)
  (evil-make-overriding-map mingus-help-map)
  (evil-make-overriding-map mingus-browse-map))

(use-package! deft
  :config
  (setq deft-extensions '("org"))
  (setq deft-directory "~/Notes")
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

(setq langtool-java-classpath
      "/usr/share/languagetool:/usr/share/java/languagetool/*")
(setq-default ispell-program-name "aspell")
(ispell-change-dictionary "american" t)

(use-package! wucuo
  :hook
(prog-mode-hook . 'wucuo-start)
(text-mode-hook . 'wucuo-start)
  )

;; emoji
;;(use-package! emojify
;;  :init
;;  (global-emojify-mode))

(use-package! rime
  :custom
  (default-input-method "rime")
  :config
  (setq rime-disable-predicates
      '(rime-predicate-evil-mode-p
        rime-predicate-after-alphabet-char-p
        rime-predicate-space-after-cc-p
        rime-predicate-space-after-ascii-p
        rime-predicate-after-ascii-char-p
        rime-predicate-tex-math-or-command-p
        rime-predicate-prog-in-code-p))
  (setq rime-show-candidate 'minibuffer)
  (define-key rime-mode-map (kbd "M-i") 'rime-force-enable))

;; (setq cnfonts--custom-set-fontnames
;;       '(("Iosevka" "SourceCodePro" "DejaVu Sans Mono")
;;         ("文泉驿等宽微米黑" "Ubuntu Mono" "隶书" "新宋体")))
;;
;; (setq cnfonts--custom-set-fontsizes
;;       '((9    9.0  9.5 )
;;         (10   11.0 11.0)
;;         (11.5 12.5 12.5)
;;         (12.5 13.5 13.5)
;;         (14   15.0 15.0)
;;         (16   17.0 17.0)
;;         (18   18.0 18.0)
;;         (20   21.0 21.0)
;;         (22   23.0 23.0)
;;         (24   25.5 25.5)
;;         (26   27.0 27.0)
;;         (28   29.0 29.0)
;;         (30   32.0 32.0)
;;         (32   33.0 33.0)))
;;
;; (setq cnfonts-profiles
;;     '("program" "org-mode"))
;;
;; (setq cnfonts-use-face-font-rescale t)
;; (cnfonts-enable)

(use-package! telega
  :load-path  "~/Applications/telega.el"
  :commands (telega)
  :defer t
  :config
  (setq telega-proxies
        (list
         '(:server "127.0.0.1" :port 1080 :enable t
                   :type (:@type "proxyTypeSocks5"))
         )))

;; (use-package! nyan-mode
;;   :config
;;   (setq nyan-animate-nyancat t)
;;   (setq nyan-wavy-trail t))
;;
;; (add-hook! 'prog-mode-hook 'nyan-mode)

(load-file "~/.doom.d/keymap.el")
