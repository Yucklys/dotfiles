(setq calendar-location-name "Beijing, CN")
(setq calendar-latitude 39.92)
(setq calendar-longitude 116.46)
(use-package! circadian
  :config
  (setq circadian-themes '(("8:00" . doom-nord-light)
                           ("19:30" . doom-nord)))
  (circadian-setup))
(setq fancy-splash-image
      (let* ((banners (directory-files "~/.doom.d/banner" 'full (rx ".png" eos)))
             (banner (elt banners (random (length banners)))))
        banner))
(set-default-font "CascadiaCode-14")
(global-prettify-symbols-mode t)

(global-set-key (kbd "<f5>") 'revert-buffer)
(global-git-gutter-mode +1)

(defun cascadia-code-mode--make-alist (list)
  "Generate prettify-symbols alist from LIST."
  (let ((idx -1))
    (mapcar
     (lambda (s)
       (setq idx (1+ idx))
       (let* ((code (+ #Xe100 idx))
              (width (string-width s))
              (prefix ())
              (suffix '(?\s (Br . Br)))
              (n 1))
         (while (< n width)
           (setq prefix (append prefix '(?\s (Br . Bl))))
           (setq n (1+ n)))
         (cons s (append prefix suffix (list (decode-char 'ucs code))))))
     list)))


(defconst cascadia-code-mode--ligatures
  '("x" "www" "*" ":" "-" "--" "---" "-->" "-|" "->"
    "->>" "--<" "-<<" "-~" "{|" ")#" "[|" "]#" "[|" "]#"
    "..." "..=" "..<" ".?" ".=" "::" ":::" "::=" ":=" ":>"
    ":<" ";;" "!!" "!!." "!=" "!==" "?." "?:" "??" "?="
    "**" "***" "*>" "*/" "#(" "#{" "#[" "#:" "#!" "#?"
    "##" "###" "####" "#=" "#_" "#_(" "/*" "/=" "/==" "/>"
    "//" "///" "_|_" "__" "+" "@" "&&" "|-" "|}" "|]"
    "||" "|||>" "||=" "||>" "|=" "|>" "$>" "++" "+++" "+>"
    "=:=" "=!=" "==" "===" "==>" "=>" "=>>" "=<<" "=/=" ">-"
    ">->" ">:" ">=" ">=>" ">>" ">>-" ">>=" ">>>" "<-" "<--"
    "<->" "<-<" "<:" "<!--" "<*" "<*>" "<|" "<||" "<|||" "<|>"
    "<$" "<$>" "<+" "<+>" "<=" "<==" "<==>" "<=>" "<=<" "<>"
    "<<" "<<-" "<<=" "<<<" "<~" "<~>" "<~~" "</" "</>" "~-"
    "~@" "~=" "~>" "~~" "~~>" "^=" "%%"))

(defvar cascadia-code-mode--old-prettify-alist)

(defun cascadia-code-mode--enable ()
  "Enable Cascadia Code ligatures in current buffer."
  (setq-local cascadia-code-mode--old-prettify-alist prettify-symbols-alist)
  (setq-local prettify-symbols-alist (append (cascadia-code-mode--make-alist cascadia-code-mode--ligatures) cascadia-code-mode--old-prettify-alist))
  (prettify-symbols-mode t))

(defun cascadia-code-mode--disable ()
  "Disable Cascadia Code ligatures in current buffer."
  (setq-local prettify-symbols-alist cascadia-code-mode--old-prettify-alist)
  (prettify-symbols-mode -1))

(define-minor-mode cascadia-code-mode
  "Cascadia Code ligatures minor mode"
  :lighter " Cascadia Code"
  (setq-local prettify-symbols-unprettify-at-point 'right-edge)
  (if cascadia-code-mode
      (cascadia-code-mode--enable)
    (cascadia-code-mode--disable)))

(defun cascadia-code-mode--setup ()
  "Setup Cascadia Code Symbols"
  (set-fontset-font t '(#Xe100 . #Xe16f) "Cascadia Code"))

(provide 'cascadia-code-mode)
(add-hook 'prog-mode-hook 'cascadia-code-mode)

(company-quickhelp-mode)

(use-package! ivy-posframe
  :after (ivy)
  :config
  (setq ivy-posframe-height-alist '((swiper . 10)
                                    (t      . 20)))
  (setq ivy-posframe-font "CascadiaCode-13")
  (setq ivy-posframe-display-functions-alist
        '((swiper          . ivy-posframe-display-at-point)
          (switch-to-buffer . ivy-posframe-display-at-point)
          (counsel-M-x     . ivy-posframe-display-at-window-center)
          (counsel-find-file . ivy-posframe-display-at-window-center)
          (t               . ivy-posframe-display-at-frame-top-center)))
  (ivy-posframe-mode 1))

(add-hook 'org-mode-hook 'org-indent-mode)
(setq org-hide-emphasis-markers nil)
(add-hook 'org-mode-hook (lambda () (display-line-numbers-mode 0)))
(use-package! darkroom
  :bind ([f9] . darkroom-mode)
  :hook
  (darkroom-mode . org-toggle-narrow-to-subtree)
  (darkroom-mode . toggle-frame-fullscreen))

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

(use-package! org-capture
  :config
  (setq org-capture-templates '(
                                ("t" "Todo" entry (file+headline "todo.org" "Task")
                                 "** TODO %?\n")
                                ("d" "日记" entry (file+olp+datetree "diary.org")
                                 "* %?\n")
                                ("i" "灵感" entry (file+headline "i.org" "Idea")
                                 "** %?\n%U\n")
                                ("r" "读书笔记" entry (file+headline "books.org" "未分类")
                                 "* %U\n** 摘录\n#+BEGIN_QUOTE\n%?\n#+END_QUOTE\n** 笔记\n")
                                ("b" "博客" entry (file+olp "blog.org" "Blog posts")
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

(use-package! elfeed-goodies
  :config
  (elfeed-goodies/setup))

(use-package! elfeed-org
  :config
  (elfeed-org)
  (setq rmh-elfeed-org-files (list "~/Notes/elfeed.org")))

(use-package! company
  :hook
  (prog-mode . company-mode)
  (org-mode . (lambda () (company-mode 0)))
  (lsp-mode . (lambda () (add-to-list 'company-backends '(company-lsp :with company-yasnippet :separate))))
  :config
  (setq company-idle-delay 0)
  (setq company-tooltip-align-annotations t)
  (setq company-minimum-prefix-length 3))

;;(use-package! eglot
;;  :hook
;;  (rustic-mode . eglot-ensure))

(use-package! company-box
  :hook (company-mode . company-box-mode))

(use-package! rainbow-delimiters
  :hook
  (prog-mode . rainbow-delimiters-mode))

(use-package! lsp-mode
  :commands lsp
  :hook
  (rust-mode . lsp)
  :config
  (setq lsp-enable-snippet t)
  (setq lsp-rust-clippy-preference "off")
  (setq lsp-enable-semantic-highlighting t))
(use-package! lsp-ui
  :hook
  (lsp-mode . lsp-ui-mode))

(use-package! py-autopep8
  :hook
  (python-mode . py-autopep8-enable-on-save))

(use-package! elpy
  :defer t
  :init
  (advice-add 'python-mode :before 'elpy-enable))

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

;; emoji
;;(use-package! emojify
;;  :init
;;  (global-emojify-mode))

(setq load-path (cons (file-truename "~/Applications/liberime") load-path))
(use-package! liberime-config
  :config
  (liberime-start "/usr/share/rime-data/" (file-truename "~/.emacs.d/rime/"))
  (liberime-select-schema "flypy"))
(use-package! pyim
  :demand t
  :config
  (setq default-input-method "pyim")
  (add-to-list 'pyim-schemes '(rime-flypy
                               :document "小鹤音形支持。"
                               :class rime
                               :first-chars "abcdefghijklmnopqrstuvwxyz"
                               :rest-chars "abcdefghijklmnopqrstuvwxyz"
                               :code-split-length 4
                               :code-maximum-length 4
                               :prefer-trigger-chars nil))
  (setq-default pyim-english-input-switch-functions
                '(pyim-probe-isearch-mode
                  pyim-probe-dynamic-english
                  pyim-probe-program-mode
                  pyim-probe-org-structure-template
                  pyim-probe-org-speed-commands))
  (setq-default pyim-punctuation-half-width-functions
                '(pyim-probe-punctuation-line-beginning
                  pyim-probe-punctuation-after-punctuation))
  (pyim-isearch-mode 1)
  (setq pyim-page-tooltip 'posframe)
  (setq pyim-page-length 5)
  (setq pyim-default-scheme 'rime-flypy)
  (setq pyim-page-style 'one-line))

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

(use-package! nyan-mode
  :config
  (setq nyan-animate-nyancat t)
  (setq nyan-wavy-trail t))

(add-hook! 'prog-mode-hook 'nyan-mode)

(load-file "~/.doom.d/keymap.el")
