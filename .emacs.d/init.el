;; Straight.el bootstrap
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package) ; Load use-package

;; (setq use-package-compute-statistics t)

;; (use-package benchmark-init
;;   :straight t
;;   :config
;;   ;; To disable collection of benchmark data after init is done.
;;   (add-hook 'after-init-hook 'benchmark-init/deactivate))

(use-package org-auto-tangle
  :straight t
  :defer t
  :hook
  (org-mode . org-auto-tangle-mode)
  :config
  (setq org-auto-tangle-babel-safelist '(
                                         "~/.emacs.d/config.org"
                                         )))

(use-package no-littering
  :straight t
  :config
  (setq custom-file (no-littering-expand-etc-file-name "custom.el"))
  (when (fboundp 'startup-redirect-eln-cache)
    (startup-redirect-eln-cache
     (convert-standard-filename
      (expand-file-name  "var/eln-cache/" user-emacs-directory)))))

(use-package emacs
  :bind
  ("C-c f p" . my/find-file-in-private-config)
  ("C-c q r" . 'restart-emacs)
  :custom
  ;; TAB cycle if there are only few candidates
  (completion-cycle-threshold 3)

  ;; Emacs 28: Hide commands in M-x which do not apply to the current mode.
  ;; Corfu commands are hidden, since they are not supposed to be used via M-x.
  (read-extended-command-predicate
   #'command-completion-default-include-p)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (tab-always-indent 'complete)

  ;; Enable recursive minibuffers
  (enable-recursive-minibuffers t)

  ;; Disable popup confirmations
  (use-dialog-box nil)
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Remember and restore the last cursor location of opened files
  ;; (save-place-mode 1)
  ;; (setq save-place-file (concat user-emacs-directory "var/saveplace"))

  ;; Revert buffers when the underlying file has changed
  (global-auto-revert-mode 1)
  :config
  (defalias 'yes-or-no-p 'y-or-n-p)
  ;; Disable auto-save
  (setq auto-save-default nil)
  )

(use-package magit
  :straight t
  :bind
  ("C-c v /"   ("Magit dispatch" . magit-dispatch)
   "C-c v ."   ("Magit file dispatch" . magit-file-dispatch)
   "C-c v '"   ("Forge dispatch" . forge-dispatch)
   "C-c v g"   ("Magit status" . magit-status)
   "C-c v G"   ("Magit status here" . magit-status-here)
   "C-c v x"   ("Magit file delete" . magit-file-delete)
   "C-c v B"   ("Magit blame" . magit-blame-addition)
   "C-c v C"   ("Magit clone" . magit-clone)
   "C-c v F"   ("Magit fetch" . magit-fetch)
   "C-c v L"   ("Magit buffer log" . magit-log-buffer-file)
   "C-c v S"   ("Git stage file" . magit-stage-file)
   "C-c v U"   ("Git unstage file" . magit-unstage-file)
   ))

(use-package corfu
  :straight t
  :custom
  (corfu-cycle t)
  (corfu-quit-no-match 'separator)
  (corfu-preselect 'prompt)
  (corfu-auto nil)
  (corfu-auto-delay 0.3)
  (corfu-auto-prefix 0)

  :hook
  (meow-insert-exit . corfu-quit)

  :bind
  (:map corfu-map
        ("S-SPC" . corfu-insert-separator)
        ("TAB" . corfu-next)
        ([tab] . corfu-next)
        ("S-TAB" . corfu-previous)
        ([backtab] . corfu-previous))

  :init
  (global-corfu-mode)

  :config
  ;; Config for tab-and-go style
  (dolist (c (list (cons "SPC" " ")
                 (cons "," ",")
                 (cons ")" ")")
                 (cons "}" "}")
                 (cons "]" "]")))
  (define-key corfu-map (kbd (car c)) `(lambda ()
                                         (interactive)
                                         (corfu-insert)
                                         (insert ,(cdr c)))))
  )

;; Use corfu even in ternimal
(use-package corfu-terminal
  :straight t
  :after corfu
  :init
  (unless (display-graphic-p)
    (corfu-terminal-mode +1)))

;; Show doc of selected candidate
(use-package corfu-popupinfo
  :load-path "straight/build/corfu/extensions/"
  :hook (corfu-mode . corfu-popupinfo-mode))

(use-package svg-lib :straight t)
(use-package kind-icon
  :straight t
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default)
  (kind-icon-default-style
   '(:padding -1 :stroke 0 :margin 0 :radius 0 :height 0.5 :scale 1.0))
  (kind-icon-blend-background nil)
  :config
  ;; Enable blend background in GUI
  (when (display-graphic-p)
    (setq kind-icon-blend-background t))

  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)
  ;; :config
  ;; (add-hook 'corfu-mode-hook
  ;;           (lambda ()
  ;;             (setq completion-in-region-function
  ;;                   (kind-icon-enhance-completion
  ;;                    completion-in-region-function))))
  )

;; Add extensions
(use-package cape
  :straight t
  :after corfu
  ;; Bind dedicated completion commands
  ;; Alternative prefix keys: C-c p, M-p, M-+, ...
  :bind (("M-p p" . completion-at-point) ;; capf
         ("M-p t" . complete-tag)        ;; etags
         ("M-p d" . cape-dabbrev)        ;; or dabbrev-completion
         ("M-p h" . cape-history)
         ("M-p f" . cape-file)
         ("M-p k" . cape-keyword)
         ("M-p s" . cape-symbol)
         ("M-p a" . cape-abbrev)
         ("M-p l" . cape-line)
         ("M-p w" . cape-dict)
         ("M-p \\" . cape-tex)
         ("M-p _" . cape-tex)
         ("M-p ^" . cape-tex)
         ("M-p &" . cape-sgml)
         ("M-p r" . cape-rfc1345))
  :init
  ;; Add `completion-at-point-functions', used by `completion-at-point'.
  ;; NOTE: The order matters!
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block)
  ;;(add-to-list 'completion-at-point-functions #'cape-history)
  ;;(add-to-list 'completion-at-point-functions #'cape-keyword)
  ;;(add-to-list 'completion-at-point-functions #'cape-tex)
  ;;(add-to-list 'completion-at-point-functions #'cape-sgml)
  ;;(add-to-list 'completion-at-point-functions #'cape-rfc1345)
  ;;(add-to-list 'completion-at-point-functions #'cape-abbrev)
  ;;(add-to-list 'completion-at-point-functions #'cape-dict)
  ;;(add-to-list 'completion-at-point-functions #'cape-symbol)
  ;;(add-to-list 'completion-at-point-functions #'cape-line)
  )

(use-package vertico
  :straight t
  :init
  (vertico-mode))

;; Optionally use the `orderless' completion style.
(use-package orderless
  :straight t
  :config
  (defun orderless-fast-dispatch (word index total)
    (and (= index 0) (= total 1) (length< word 4)
       `(orderless-regexp . ,(concat "^" (regexp-quote word)))))

  (orderless-define-completion-style orderless-fast
    (orderless-style-dispatchers '(orderless-fast-dispatch))
    (orderless-matching-styles '(orderless-literal orderless-regexp)))

  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles . (partial-completion)))
  				      (symbol (styles . (orderless-fast)))))
  )

;; Support Pinyin with pinyinlib
(use-package pinyinlib
  :straight t
  :config
  (defun completion--regex-pinyin (str)
    (orderless-regexp (pinyinlib-build-regexp-string str)))
  (add-to-list 'orderless-matching-styles 'completion--regex-pinyin))

;; Enable rich annotations using the Marginalia package
(use-package marginalia
  :straight t
  ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
  ;; available in the *Completions* buffer, add it to the
  ;; `completion-list-mode-map'.
  :bind (:map minibuffer-local-map
         ("M-A" . marginalia-cycle))

  ;; The :init section is always executed.
  :init

  ;; Marginalia must be actived in the :init section of use-package such that
  ;; the mode gets enabled right away. Note that this forces loading the
  ;; package.
  (marginalia-mode))

(use-package embark
  :straight t
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)
   :map embark-org-link-map
   ("RET" . org-open-at-point-global)
   ("o"   . jv-org-open-link-string-in-side-window))
  ;; alternative for `describe-bindings'
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  ;; Show the Embark target at point via Eldoc.  You may adjust the Eldoc
  ;; strategy, if you want to see the documentation from multiple providers.
  (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)
  :config
  ;; Open the link in the side window using embark-act
  (defun jv-get-create-side-window ()
    "Return side window, or create one."
    (when (one-window-p)
      (split-window-horizontally))
    (or (window-in-direction 'right)
        (window-in-direction 'left)
        (selected-window)))
  ;; teach embark to visit org links:
  (defun embark-target-org-link-at-point ()
    "Teach embark to reconize org links at point."
    (when (org-in-regexp org-link-any-re)
      (cons 'org-link (match-string-no-properties 0))))
  (defun jv-org-open-link-string-in-side-window (s)
    (select-window (jv-get-create-side-window))
    (org-link-open-from-string s))

  (advice-add 'org-open-at-point-global :before #'push-mark)
  (add-to-list 'embark-target-finders
               #'embark-target-org-link-at-point)
  (add-to-list 'embark-keymap-alist
               '(org-link . embark-org-link-map))

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package tempel
  :straight t
  ;; Require trigger prefix before template name when completing.
  ;; :custom
  ;; (tempel-trigger-prefix "<")

  :bind (("M-+" . tempel-complete) ;; Alternative tempel-expand
         ("M-*" . tempel-insert)
         ("M-]" . tempel-next)
         ("M-[" . tempel-prev))

  :init

  ;; Setup completion at point
  (defun tempel-setup-capf ()
    ;; Add the Tempel Capf to `completion-at-point-functions'.
    ;; `tempel-expand' only triggers on exact matches. Alternatively use
    ;; `tempel-complete' if you want to see all matches, but then you
    ;; should also configure `tempel-trigger-prefix', such that Tempel
    ;; does not trigger too often when you don't expect it. NOTE: We add
    ;; `tempel-expand' *before* the main programming mode Capf, such
    ;; that it will be tried first.
    (setq-local completion-at-point-functions
                (cons #'tempel-expand
                      completion-at-point-functions)))

  (add-hook 'conf-mode-hook 'tempel-setup-capf)
  (add-hook 'prog-mode-hook 'tempel-setup-capf)
  (add-hook 'text-mode-hook 'tempel-setup-capf)

  ;; Optionally make the Tempel templates available to Abbrev,
  ;; either locally or globally. `expand-abbrev' is bound to C-x '.
  ;; (add-hook 'prog-mode-hook #'tempel-abbrev-mode)
  ;; (global-tempel-abbrev-mode)
  )

;; Optional: Add tempel-collection.
;; The package is young and doesn't have comprehensive coverage.
(use-package tempel-collection
  :straight t
  :after tempel)

(use-package consult
  :straight t
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :bind (("C-x b"   . 'my/consult-buffer)
       ("C-x B"   . 'consult-buffer)
         ("C-c j i" . 'consult-imenu)
         ("C-c j b" . 'consult-bookmark)
         ("C-c j m" . 'consult-mark)
         ("C-c j o" . 'consult-outline)
         ("C-c j r" . 'consult-recent-file)
         ("C-c j l" . 'consult-line)
         ("C-c j L" . 'consult-line-multi)
         ("C-c j g" . 'consult-ripgrep)
         ("C-c j f" . 'consult-find)
         ("C-c j F" . 'consult-locate)
         ("C-c j h" . 'consult-complex-command)
         ("C-c j c" . 'consult-mode-command)
         ("C-c j a" . 'consult-org-agenda)
         ("C-c s f" . 'consult-focus-lines)
         ("C-c s m" . 'consult-minor-mode-menu)
         :map org-mode-map
         ("C-c j o" . 'consult-org-heading)
         :map help-map
         ("t" . 'consult-theme))
  :init
  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  (defun my/consult-buffer ()
    "Use `consult-project-buffer' when inside a project, otherwise use `consult-buffer'"
    (interactive)
    (if (project-current)
      (consult-project-buffer)
      (consult-buffer)))

  :config
  (setq consult-buffer-filter `(,@consult-buffer-filter
  			      "\\`\\*Async-native-compile-log\\*\\'"
  			      "\\`\\*straight-process\\*\\'"
  			      "\\`\\*dashboard\\*\\'"
  			      "\\`\\*.*\\*\\'"))
  (setq-default consult-preview-key 'any)
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))

  ;; Load projectile projects
  (autoload 'projectile-project-root "projectile")
  (setq consult-project-function (lambda (_) (projectile-project-root)))

  (defun consult-info-emacs ()
    "Search through Emacs info pages."
    (interactive)
    (consult-info "emacs" "efaq" "elisp" "cl" "compat"))

  (defun consult-info-org ()
    "Search through the Org info page."
    (interactive)
    (consult-info "org"))

  (defun consult-info-completion ()
    "Search through completion info pages."
    (interactive)
    (consult-info "vertico" "consult" "marginalia" "orderless" "embark"
                  "corfu" "cape" "tempel")))

;; Enable when use with embark
(use-package embark-consult
  :straight t ; only need to install it, embark loads it after consult if found
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;; Integrate with Org roam
(use-package consult-org-roam
  :straight t
  :after org-roam
  :init
  (require 'consult-org-roam)
  (consult-org-roam-mode 1)
  :custom
  ;; Use `ripgrep' for searching with `consult-org-roam-search'
  (consult-org-roam-grep-func #'consult-ripgrep)
  ;; Configure a custom narrow key for `consult-buffer'
  (consult-org-roam-buffer-narrow-key ?r)
  ;; Display org-roam buffers right after non-org-roam buffers
  ;; in consult-buffer (and not down at the bottom)
  (consult-org-roam-buffer-after-buffers t)
  :config
  ;; Eventually suppress previewing for certain functions
  ;; (consult-customize
  ;;  consult-org-roam-forward-links
  ;;  :preview-key (kbd "M-."))
  :bind
  ;; Define some convenient keybindings as an addition
  (("C-c n f" . consult-org-roam-file-find)
   ("C-c n b" . consult-org-roam-backlinks)
   ("C-c n l" . consult-org-roam-forward-links)
   ("C-c n s" . consult-org-roam-search)))

;; Integrate with flycheck
(use-package consult-flycheck
  :straight t
  :after (flycheck consult)
  :bind
  ("C-c e e" . 'consult-flycheck))

;; Integrate with eglot
(use-package consult-eglot
  :straight t
  :defer t
  :bind
  (:map eglot-mode-map
      ([remap xref-find-apropos] . #'consult-eglot-symbols))
  )

;; Integrate with projectile
(use-package consult-projectile
  :straight (consult-projectile
  	   :type git :host gitlab
  	   :repo "OlMon/consult-projectile" :branch "master")
  :defer t
  :bind
  ("C-c p p" . 'consult-projectile-switch-project)
  ("C-c p b" . 'consult-project-buffer)
  ("C-c p e" . 'consult-projectile-recentf)
  ("C-c p f" . 'consult-projectile-find-file)
  ("C-c p d" . 'consult-projectile-find-dir)
  )

(use-package dash
  :straight t)

(defun my/find-file-in-private-config ()
  "Search for a file in `doom-user-dir'."
  (interactive)
  (dired-find-file user-emacs-directory))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :defer t
  :custom
  (history-length 25)
  :init
  (savehist-mode 1))

(use-package recentf
  :after (no-littering org)
  :config
    ;; Put all recentf files together
  (add-to-list 'recentf-exclude
               (recentf-expand-file-name no-littering-var-directory))
  (add-to-list 'recentf-exclude
               (recentf-expand-file-name no-littering-etc-directory))
  (add-to-list 'recentf-exclude
               (concat org-directory "todo.org"))
  (add-to-list 'recentf-exclude
               (concat org-directory "index.org")))

(use-package centaur-tabs
  :straight t
  :bind
  ("C-c t n" . 'centaur-tabs-forward-tab)
  ("C-c t p" . 'centaur-tabs-backward-tab)
  ("C-c t N" . 'centaur-tabs-select-end-tab)
  ("C-c t P" . 'centaur-tabs-select-beg-tab)
  ("C-c t s" . 'centaur-tabs-switch-group)
  ("C-c t j" . 'centaur-tabs-ace-jump)
  :hook
  (org-src-mode . centaur-tabs-local-mode) ; disable bar in org edit src
  (dashboard-mode . centaur-tabs-local-mode)
  (term-mode . centaur-tabs-local-mode)
  (org-agenda-mode . centaur-tabs-local-mode)
  (calendar-mode . centaur-tabs-local-mode)

  :init        
  (defun centaur-tabs-buffer-groups ()
    "`centaur-tabs-buffer-groups' control buffers' group rules.

Group centaur-tabs with mode if buffer is derived from `eshell-mode' `emacs-lisp-mode' `dired-mode' `org-mode' `magit-mode'.
All buffer name start with * will group to \"Emacs\".
Other buffer group by `centaur-tabs-get-group-name' with project name."
    (list
     (cond
      ((or (string-equal "*" (substring (buffer-name) 0 1))
           (memq major-mode '(magit-process-mode
                              magit-status-mode
                              magit-diff-mode
                              magit-log-mode
                              magit-file-mode
                              magit-blob-mode
                              magit-blame-mode
                              )))
       "Emacs")
      ((derived-mode-p 'prog-mode)
       "Editing")
      ((derived-mode-p '(eshell-mode
  		       vterm-mode))
       "Term")
      ((derived-mode-p 'dired-mode)
       "Dired")
      ((memq major-mode '(helpful-mode
                          help-mode))
       "Help")
      ((memq major-mode '(org-mode
  			org-roam-mode
                          org-agenda-clockreport-mode
                          org-src-mode
                          org-agenda-mode
                          org-beamer-mode
                          org-indent-mode
                          org-bullets-mode
                          org-cdlatex-mode
                          org-agenda-log-mode
                          diary-mode))
       "Org")
      (t
       (centaur-tabs-get-group-name (current-buffer))))))

  (defun centaur-tabs-hide-tab (x)
    "Do no to show buffer X in tabs."
    (let ((name (format "%s" x)))
      (or
       ;; Current window is not dedicated window.
       (window-dedicated-p (selected-window))

       ;; Buffer name not match below blacklist.
       (string-prefix-p "*epc" name)
       (string-prefix-p "*helm" name)
       (string-prefix-p "*Helm" name)
       (string-prefix-p "*Compile-Log*" name)
       (string-prefix-p "*lsp" name)
       (string-prefix-p "*company" name)
       (string-prefix-p "*Flycheck" name)
       (string-prefix-p "*tramp" name)
       (string-prefix-p " *Mini" name)
       (string-prefix-p "*help" name)
       (string-prefix-p "*straight" name)
       (string-prefix-p " *temp" name)
       (string-prefix-p "*Help" name)
       (string-prefix-p "*mybuf" name)

       ;; Is not magit buffer.
       (and (string-prefix-p "magit" name)
            (not (file-name-extension name)))
       )))
  (centaur-tabs-mode t)
  (centaur-tabs-headline-match)

  :config
  ;; Tab appearence
  (setq centaur-tabs-style "bar")
  (setq centaur-tabs-height 32)
  (setq centaur-tabs-set-icons t)
  (setq centaur-tabs-set-bar 'under)
  (setq x-underline-at-descent-line t)
  (setq centaur-tabs-set-close-button nil)
  (setq centaur-tabs-set-modified-marker t)

  ;; Customize
  (setq centaur-tabs-cycle-scope 'tabs) ; tabs or groups
  (setq centaur-tabs--buffer-show-groups nil)
  (centaur-tabs-enable-buffer-reordering)
  (setq centaur-tabs-adjust-buffer-order t)

  ;; Integration
  (centaur-tabs-group-by-projectile-project)

  ;; Custome face
  (set-face-attribute 'centaur-tabs-selected nil
  		    :inherit 'centaur-tabs-selected
  		    :underline "#81A1C1")
  (set-face-attribute 'centaur-tabs-selected-modified nil
  		    :inherit 'centaur-tabs-selected
  		    :foreground "#8FBCBB"
  		    :underline "#81A1C1")
  (set-face-attribute 'centaur-tabs-default nil
  		    :inherit 'centaur-tabs-default
  		    :background "#3B4252")
  )

;; (use-package tab-bar
;;   :bind
;;   ("C-c b n" . 'tab-bar-switch-to-next-tab)
;;   ("C-c b p" . 'tab-bar-switch-to-prev-tab)
;;   ("C-c b N" . 'tab-bar-switch-to-last-tab)
;;   ("C-c b b" . 'tab-bar-switch-to-tab)
;;   ("C-c b k" . 'tab-bar-close-tab)
;;   ("C-c b K" . 'tab-bar-close-tab-by-name)
;;   :init
;;   (tab-bar-mode +1)
;;   :custom
;;   ;; Only show the tab bar if there are 2 or more tabs
;;   (tab-bar-show t)
;;   ;; Do not show buttons
;;   (tab-bar-new-button-show nil)
;;   (tab-bar-close-button-show nil)
;;   ;; Show hint index
;;   ;; Open new tab rightmost
;;   (tab-bar-new-tab-to "rightmost")

;;   ;; Custom tab name and group format
;;   (tab-bar-tab-name-format-function #'my/tab-bar-format)
;;   (tab-bar-tab-name-function #'my/tab-bar-tab-name)
;;   ;; (tab-bar-tab-group-format-function #'hyphon-tab-bar-tab-group-format-default)

;;   ;; Open dashboard with new tab
;;   (tab-bar-new-tab-choice #'dashboard-open)

;;   ;; (tab-bar-format '(tab-bar-format-tabs-groups tab-bar-separator))
;;   :config  
;;   (defun my/tab-bar-format (tab i)
;;     (let ((current-p (eq (car tab) 'current-tab)))
;;       (propertize
;;        (concat (if tab-bar-tab-hints (format "%d " i) "")
;;                (alist-get 'name tab)
;;                (or (and tab-bar-close-button-show
;; 			(not (eq tab-bar-close-button-show
;; 				 (if current-p 'non-selected 'selected)))
;; 			tab-bar-close-button)
;;                    ""))
;;        'face (funcall tab-bar-tab-face-function tab))))

;;   (defun my/tab-bar-tab-name ()
;;     (let ((project (project-current)))
;;       (concat (if project "[" "")
;; 	      project
;; 	      (if project "] " "")
;; 	      (tab-bar-tab-name-current)))
;;     )

;;   (setq tab-bar-tab-hints nil)
;;   (setq tab-bar-auto-width nil)
;;   (setq tab-bar-format '(tab-bar-format-history
;; 			 tab-bar-format-tabs
;; 			 tab-bar-separator
;; 			 tab-bar-format-align-right
;; 			 tab-bar-format-global
;; 			 ))

;;   )

(use-package flycheck
  :straight t
  :hook
  ;; Disable emacs-lisp-checkers in org code block
  (org-src-mode . (lambda ()
                    (setq-local flycheck-disabled-checkers
                                '(emacs-lisp
                                  emacs-lisp-checkdoc))))
  :init (global-flycheck-mode))

(use-package meow
  :straight t
  :config
  (defun meow-setup ()
    (setq meow-cheatsheet-layout meow-cheatsheet-layout-dvorak)
    (meow-leader-define-key
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
     '("?" . meow-cheatsheet))
    (meow-motion-overwrite-define-key
     ;; custom keybinding for motion state
     '("<escape>" . ignore))
    (meow-normal-define-key
     '("0" . meow-expand-0)
     '("9" . meow-expand-9)
     '("8" . meow-expand-8)
     '("7" . meow-expand-7)
     '("6" . meow-expand-6)
     '("5" . meow-expand-5)
     '("4" . meow-expand-4)
     '("3" . meow-expand-3)
     '("2" . meow-expand-2)
     '("1" . meow-expand-1)
     '("-" . negative-argument)
     '(";" . meow-reverse)
     '("," . meow-inner-of-thing)
     '("." . meow-bounds-of-thing)
     '("<" . meow-beginning-of-thing)
     '(">" . meow-end-of-thing)
     '("a" . meow-append)
     '("A" . meow-open-below)
     '("b" . meow-back-word)
     '("B" . meow-back-symbol)
     '("c" . meow-change)
     '("d" . meow-delete)
     '("D" . meow-backward-delete)
     '("e" . meow-line)
     '("E" . meow-goto-line)
     '("f" . meow-find)
     '("g" . meow-cancel-selection)
     '("G" . meow-grab)
     '("h" . meow-left)
     '("H" . meow-left-expand)
     '("i" . meow-insert)
     '("I" . meow-open-above)
     '("j" . meow-join)
     '("J" . meow-page-down)
     '("K" . meow-page-up)
     '("k" . meow-kill)
     '("l" . meow-till)
     '("m" . meow-mark-word)
     '("M" . meow-mark-symbol)
     '("n" . meow-next)
     '("N" . meow-next-expand)
     '("o" . meow-block)
     '("O" . meow-to-block)
     '("p" . meow-prev)
     '("P" . meow-prev-expand)
     '("q" . meow-quit)
     '("Q" . consult-goto-line) ; Consult goto-line with live preview
     '("r" . meow-replace)
     '("R" . meow-swap-grab)
     '("s" . meow-search)
     '("t" . meow-right)
     '("T" . meow-right-expand)
     '("u" . meow-undo)
     '("U" . meow-undo-in-selection)
     '("v" . meow-visit)
     '("w" . meow-next-word)
     '("W" . meow-next-symbol)
     '("x" . meow-save)
     '("X" . meow-sync-grab)
     '("y" . meow-yank)
     '("Y" . consult-yank-from-kill-ring) ; Consult view yank history
     '("z" . meow-pop-selection)
     '("'" . repeat)
     '("<f5>" . consult-kmacro) ; Consult kmacro
     '("<escape>" . ignore)))
  (meow-setup)
  (meow-global-mode 1))

(use-package smartparens
  :straight t
  :bind
  ("C-c c w r"  . 'sp-wrap-round)
  ("C-c c w c"  . 'sp-wrap-curly)
  ("C-c c w s"  . 'sp-wrap-square)
  :hook
  (prog-mode . smartparens-mode)
  )

(use-package rime
  :straight (rime :type git
                  :host github
                  :repo "DogLooksGood/emacs-rime"
                  :files ("*.el" "Makefile" "lib.c"))
  :custom
  (default-input-method "rime")
  :config
  (defun rime-predicate-meow-mode-p ()
    "Detect whether the current buffer is in `meow' state.
    Include `meow-normal-state' ,`meow-motion-state' , `meow-keypad-state'.
    Can be used in `rime-disable-predicates' and `rime-inline-predicates'."
    (and (fboundp 'meow-mode)
         (or (meow-normal-mode-p)
             (meow-keypad-mode-p)
             (meow-motion-mode-p))))
  (setq rime-disable-predicates
        '(rime-predicate-meow-mode-p
          rime-predicate-after-alphabet-char-p
          rime-predicate-tex-math-or-command-p
          rime-predicate-punctuation-after-space-cc-p
          rime-predicate-prog-in-code-p
          ;; rime-predicate-punctuation-line-begin-p
          ;; rime-predicate-current-uppercase-letter-p
          ))
  ;; ;; (setq rime-disable-predicates nil)
  (setq rime-inline-predicates
        '(rime-predicate-space-after-cc-p))
  (setq rime-inline-ascii-trigger 'shift-l)
  (setq rime-show-candidate 'minibuffer)
  (define-key rime-mode-map (kbd "M-i") 'rime-force-enable))

;; Change keyboard layout
;; (use-package quail
;;   :config
;;   (add-to-list 'quail-keyboard-layout-alist
;;                `("dvorak" . ,(concat "                              "
;;                                      "  1!2@3#4$5%6^7&8*9(0)[{]}`~  "
;;                                      "  '\",<.>pPyYfFgGcCrRlL/?=+    "
;;                                      "  aAoOeEuUiIdDhHtTnNsS-_\\|    "
;;                                      "  ;:qQjJkKxXbBmMwWvVzZ        "
;;                                      "                              ")))
;;   (quail-set-keyboard-layout "dvorak"))

(use-package vterm
  :straight t
  :bind
  (("C-c o T" . 'vterm)
   :map vterm-mode-map
   ("C-q" . 'vterm-send-next-key))

  :config
  (setq vterm-kill-buffer-on-exit t)
  (add-hook 'vterm-mode-hook
            (lambda ()
              (set (make-local-variable 'buffer-face-mode-face)
  		 '(:height 140 :family "Iosevka Nerd Font"))
              (buffer-face-mode t)))
  )

(use-package vterm-toggle
  :straight t
  :bind
  (("C-c o t" . 'vterm-toggle)
   :map vterm-mode-map
   ("M-n" . 'vterm-toggle-forward)
   ("M-p" . 'vterm-toggle-backward))
  :config
  (setq vterm-toggle-fullscreen-p nil)
  (add-to-list 'display-buffer-alist
             '((lambda (buffer-or-name _)
                   (let ((buffer (get-buffer buffer-or-name)))
                     (with-current-buffer buffer
                       (or (equal major-mode 'vterm-mode)
                           (string-prefix-p vterm-buffer-name (buffer-name buffer))))))
               (display-buffer-reuse-window display-buffer-in-side-window)
               (side . bottom)
               ;;(dedicated . t) ;dedicated is supported in emacs27
               (reusable-frames . visible)
               (window-height . 0.3)))
  )

(use-package eglot
  :disabled t
  :init
  (setq eglot-sync-connect 1
      eglot-connect-timeout 10
      eglot-autoshutdown t
      eglot-send-changes-idle-time 0.5)
  :defer t
  :bind
  (:map eglot-mode-map
      ("C-c c a" . 'eglot-code-actions)
      ("C-c c r" . 'eglot-rename))
  :config  
  ;; Ensure completion table is refreshed such that
  ;; the candidates are always obtained again from the server.
  ;; Depending on if your server returns sufficiently many candidates in the first place.
  (setq completion-category-overrides '((eglot (styles orderless))))

  ;; Disable flymake
  ;; (add-hook 'eglot--managed-mode-hook (lambda () (flymake-mode -1)))

  ;; (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)

  ;; Combine eglot, tempel and cape-file into same place.
  ;; (defun my/eglot-capf ()
  ;;   (setq-local completion-at-point-functions
  ;; 		(list (cape-super-capf
  ;;                      #'eglot-completion-at-point
  ;;                      #'tempel-expand
  ;;                      #'cape-file))))
  ;; (add-hook 'eglot-managed-mode-hook #'my/eglot-capf)

  ;; (add-to-list 'eglot-server-programs
  ;; 	       `((rust-ts-mode rust-mode) . ("rust-analyzer" :initializationOptions
  ;; 					     )))
  )

(use-package flycheck-eglot
  :straight t
  :after (flycheck eglot)
  :config
  (global-flycheck-eglot-mode 1))


(use-package eglot-x
  :straight (eglot-x :type git :host github
  		   :repo "nemethf/eglot-x")
  :after eglot
  :config
  (eglot-x-setup))

(use-package lsp-mode
  :straight t
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  (defun my/lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless-fast))) ;; Configure orderless

  ;; Combine lsp-capf, tempel and cape-file into same place.
  (defun my/lsp-capf ()
    (setq-local completion-at-point-functions
  	      (list (cape-super-capf
                       #'tempel-expand
                       #'lsp-completion-at-point
                       #'cape-file))))

  :bind
  (:map lsp-mode-map
      ("C-c c a" . 'lsp-execute-code-action)
      ("C-c c d" . 'lsp-find-definition)
      ("C-c c i" . 'lsp-find-implementation)
      ("C-c c r" . 'lsp-rename))

  :custom
  (lsp-completion-provider :none)

  :hook
  ;; replace XXX-mode with concrete major-mode(e. g. python-mode)
  ;; (XXX-mode . lsp)
  ;; if you want which-key integration
  (lsp-mode . lsp-enable-which-key-integration)
  (lsp-mode . my/lsp-capf)
  (lsp-completion-mode . my/lsp-mode-setup-completion)

  :config
  ;; Disable default keybindings
  (setq lsp-keymap-prefix nil)
  (setq read-process-output-max (* 1024 1024)) ;; 1mb)
  (setq lsp-log-io nil)
  (setq lsp-enable-snippet nil)

  ;; Some features that have great potential to be slow.
  ;; Suggested by Doom Emacs
  (setq lsp-enable-folding nil
        lsp-enable-text-document-color nil)

  ;; Disable breadcrumbs
  (setq lsp-headerline-breadcrumb-enable nil)

  ;; Enable inlay hint
  (setq lsp-inlay-hint-enable t)
  )

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :straight t
  :config
  (setq lsp-ui-peek-enable t
        lsp-ui-doc-max-height 8
        lsp-ui-doc-max-width 72         ; 150 (default) is too wide
        lsp-ui-doc-delay 0.75           ; 0.2 (default) is too naggy
        lsp-ui-doc-show-with-mouse nil  ; don't disappear on mouseover
        lsp-ui-doc-position 'at-point
        lsp-ui-sideline-ignore-duplicate t
        ;; Don't show symbol definitions in the sideline. They are pretty noisy,
        ;; and there is a bug preventing Flycheck errors from being shown (the
        ;; errors flash briefly and then disappear).
        lsp-ui-sideline-show-hover nil
        ;; Re-enable icon scaling (it's disabled by default upstream for Emacs
        ;; 26.x compatibility; see emacs-lsp/lsp-ui#573)
        lsp-ui-sideline-actions-icon lsp-ui-sideline-actions-icon-default)
  )

(use-package lsp-treemacs
  :straight t
  :after (lsp-mode treemacs doom-themes)
  :config
  (lsp-treemacs-sync-mode 1)

  ;; Fix conflict of icon theme with doom themes
  (with-eval-after-load 'lsp-treemacs
          (doom-themes-treemacs-config))
  )

(use-package consult-lsp
  :straight t
  :defer t
  :bind
  (:map lsp-mode-map
      ([remap xref-find-apropos] . 'consult-lsp-symbols)
      )
  )

;; Show my keybindings
(use-package which-key
  :straight t
  :config
  (setq which-key-show-early-on-C-h t)
  (which-key-mode))

;; Embark which-key integration
(defun embark-which-key-indicator ()
  "An embark indicator that displays keymaps using which-key.
The which-key help message will show the type and value of the
current target followed by an ellipsis if there are further
targets."
  (lambda (&optional keymap targets prefix)
    (if (null keymap)
        (which-key--hide-popup-ignore-command)
      (which-key--show-keymap
       (if (eq (plist-get (car targets) :type) 'embark-become)
           "Become"
         (format "Act on %s '%s'%s"
                 (plist-get (car targets) :type)
                 (embark--truncate-target (plist-get (car targets) :target))
                 (if (cdr targets) "…" "")))
       (if prefix
           (pcase (lookup-key keymap prefix 'accept-default)
             ((and (pred keymapp) km) km)
             (_ (key-binding prefix 'accept-default)))
         keymap)
       nil nil t (lambda (binding)
                   (not (string-suffix-p "-argument" (cdr binding))))))))

(setq embark-indicators
      '(embark-which-key-indicator
        embark-highlight-indicator
        embark-isearch-highlight-indicator))

(defun embark-hide-which-key-indicator (fn &rest args)
  "Hide the which-key indicator immediately when using the completing-read prompter."
  (which-key--hide-popup-ignore-command)
  (let ((embark-indicators
         (remq #'embark-which-key-indicator embark-indicators)))
    (apply fn args)))

(advice-add #'embark-completing-read-prompter
            :around #'embark-hide-which-key-indicator)

;; Better other-window
(use-package ace-window
  :straight t
  :bind ("M-o" . ace-window))

;; Better project management
(use-package projectile
  :straight t
  :custom
  (projectile-sort-order 'recently-active)
  (projectile-project-search-path '("~/Projects/"))
  :config
  (projectile-mode +1))

;; (use-package project)

(use-package dirvish
  :straight t
  :hook
  (dirvish-find-entry .
                      (lambda (&rest _) (setq-local truncate-lines t)))
  :init
  ;; (dirvish-peek-mode)
  (dirvish-override-dired-mode)
  :bind
  (("C-x d"	.	dirvish)
   ("C-c f d"	.	dirvish-fd)
   :map dirvish-mode-map
   ("a"		.	dirvish-quick-access)
   ("f"		.	dirvish-file-info-menu)
   ("y"		.	dirvish-yank-menu)
   ("N"		.	dirvish-narrow)
   ("^"		.	dirvish-history-last) ; remapped `dired-up-directory'
   ("s"		.	dirvish-quicksort)    ; remapped `dired-sort-toggle-or-edit'
   ("v"		.	dirvish-vc-menu)      ; remapped `dired-view-file'
   ("h"		.	dired-up-directory)   ; remapped `describe-mode'
   ("H"		.	dirvish-history-jump)
   ("t"		.	dired-find-file)      ; remapped `dired-toggle-marks'
   ("T"		.	dired-toggle-marks)
   ("`"         .       dired-omit-mode)
   ("TAB"	.	dirvish-subtree-toggle)
   ("M-f"	.	dirvish-history-go-forward)
   ("M-b"	.	dirvish-history-go-backward)
   ("M-l"	.	dirvish-ls-switches-menu)
   ("M-m"	.	dirvish-mark-menu)
   ("M-t"	.	dirvish-layout-toggle)
   ("M-s"	.	dirvish-setup-menu)
   ("M-e"	.	dirvish-emerge-menu)
   ("M-j"	.	dirvish-fd-jump))
  :custom
  (dirvish-attributes '(all-the-icons
                        git-msg
                        collapse
                        file-size
                        file-time))
  (delete-by-moving-to-trash t) ; Delete to trash
  (dired-listing-switches
   "-l --almost-all --human-readable --group-directories-first --no-group")
  (dirvish-quick-access-entries
   '(("h" "~/"                          "Home")
     ("d" "~/Downloads/"                "Downloads")
     ("p" "~/Projects/"                 "Projects")
     ("e" "~/.emacs.d/"                 "Emacs")
     ("t" "~/.local/share/Trash/files/" "Trash Can")))
  ;; Ignore some files
  (dired-omit-files
   (rx (or (seq bol (? ".") "#")         ;; emacs autosave files
           (seq bol "." (not (any "."))) ;; dot-files
           (seq "~" eol)                 ;; backup-files
           (seq bol "CVS" eol)           ;; CVS dirs
           ))))

(use-package treemacs
  :straight (treemacs
             :type git
             :repo "Alexander-Miller/treemacs")
  :bind (("M-0"     . treemacs-select-window)
         ("C-c t 1" . treemacs-delete-other-windows)
         ("C-c t t" . treemacs)
         ("C-c t d" . treemacs-select-directory)
         ("C-c t B" . treemacs-bookmark)
         :map treemacs-mode-map
         ("f v"     . treemacs-find-file)
         ("f t"     . treemacs-find-tag))
  :custom
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-is-never-other-window t)
  (treemacs-follow-after-init t)
  (treemacs-hide-gitignored-files-mode t)
  (treemacs-sorting 'alphabetic-case-insensitive-asc)
  (treemacs-collapse-dirs 3) ; Combine empty directories into one
  :config
  ;; Recognize packages in treemacs's tag-view
  (add-to-list 'treemacs-elisp-imenu-expression
               '("Package"
                 "\\(^\\s-*(use-package +\\)\\(\\_<.+\\_>\\)" 2))
  (pcase (cons (not (null (executable-find "git")))
               (not (null treemacs-python-executable)))
    (`(t . t)
     (treemacs-git-mode 'deferred))
    (`(t . _)
     (treemacs-git-mode 'simple)))
  (treemacs-fringe-indicator-mode 'always))

;; (use-package treemacs-tab-bar
;;   :straight t
;;   :after (treemacs))

(use-package treemacs-projectile
  :straight t
  :after (treemacs projectile))

(use-package imenu
  :config
  ;; Create imenu menu for use-package
  (add-to-list 'imenu-generic-expression
               '("Package"
                 "\\(^\\s-*(use-package +\\)\\(\\_<.+\\_>\\)" 2)))

;; Raise gc-cons-threashold while the minibuffer is active
;; Borrow from Doom Emacs
(defun doom-defer-garbage-collection-h ()
  (setq gc-cons-threshold most-positive-fixnum))

(defun doom-restore-garbage-collection-h ()
  ;; Defer it so that commands launched immediately after will enjoy the
  ;; benefits.
  (run-at-time
   1 nil (lambda () (setq gc-cons-threshold 800000))))

(add-hook 'minibuffer-setup-hook #'doom-defer-garbage-collection-h)
(add-hook 'minibuffer-exit-hook #'doom-restore-garbage-collection-h)

(use-package doom-themes
  :straight t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (setq custom-safe-themes t)
  (load-theme 'doom-nord t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  ;; (doom-themes-neotree-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-colors") ; use "doom-colors" for less minimal icon theme
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package doom-modeline
  :straight t
  :init (doom-modeline-mode 1)
  :custom
  (doom-modeline-hud t) ; Disable graphical modeline
  (doom-modeline-modal t) ; Show INSERT/NORMAL for modal editor
  (doom-modeline-modal-icon t) ; Show icons for modal editor
  (doom-modeline-height 32) ; Set the height of modeline
  ;; (doom-modeline-display-default-persp-name t)
  )

(use-package dashboard
  :straight t
  :custom
  (dashboard-startup-banner '1)
  (dashboard-projects-backend 'projectile) ; Get projects from projectile
  ;; (dashboard-page-separator "\n\f\n")      ; Use page-break-lines
  (dashboard-center-content t)             ; Put content right
  (dashboard-agenda-release-buffers t)
  :config
  (add-to-list 'dashboard-items '(projects . 5) t)
  (dashboard-setup-startup-hook)
  (setq initial-buffer-choice
        (lambda () (get-buffer-create "*dashboard*"))) ; Show dashboard with emacsclient
  )

;; All-the-icons
(use-package all-the-icons
  :straight t
  :if (display-graphic-p))

;; Nerd icons for terminal support
(use-package nerd-icons
  :straight t
  :config
  (setq kind-icon-use-icons nil)
  (setq kind-icon-mapping
        `(
          (array ,(nerd-icons-codicon "nf-cod-symbol_array") :face font-lock-type-face)
          (boolean ,(nerd-icons-codicon "nf-cod-symbol_boolean") :face font-lock-builtin-face)
          (class ,(nerd-icons-codicon "nf-cod-symbol_class") :face font-lock-type-face)
          (color ,(nerd-icons-codicon "nf-cod-symbol_color") :face success)
          (command ,(nerd-icons-codicon "nf-cod-terminal") :face default)
          (constant ,(nerd-icons-codicon "nf-cod-symbol_constant") :face font-lock-constant-face)
          (constructor ,(nerd-icons-codicon "nf-cod-triangle_right") :face font-lock-function-name-face)
          (enummember ,(nerd-icons-codicon "nf-cod-symbol_enum_member") :face font-lock-builtin-face)
          (enum-member ,(nerd-icons-codicon "nf-cod-symbol_enum_member") :face font-lock-builtin-face)
          (enum ,(nerd-icons-codicon "nf-cod-symbol_enum") :face font-lock-builtin-face)
          (event ,(nerd-icons-codicon "nf-cod-symbol_event") :face font-lock-warning-face)
          (field ,(nerd-icons-codicon "nf-cod-symbol_field") :face font-lock-variable-name-face)
          (file ,(nerd-icons-codicon "nf-cod-symbol_file") :face font-lock-string-face)
          (folder ,(nerd-icons-codicon "nf-cod-folder") :face font-lock-doc-face)
          (interface ,(nerd-icons-codicon "nf-cod-symbol_interface") :face font-lock-type-face)
          (keyword ,(nerd-icons-codicon "nf-cod-symbol_keyword") :face font-lock-keyword-face)
          (macro ,(nerd-icons-codicon "nf-cod-symbol_misc") :face font-lock-keyword-face)
          (magic ,(nerd-icons-codicon "nf-cod-wand") :face font-lock-builtin-face)
          (method ,(nerd-icons-codicon "nf-cod-symbol_method") :face font-lock-function-name-face)
          (function ,(nerd-icons-codicon "nf-cod-symbol_method") :face font-lock-function-name-face)
          (module ,(nerd-icons-codicon "nf-cod-file_submodule") :face font-lock-preprocessor-face)
          (numeric ,(nerd-icons-codicon "nf-cod-symbol_numeric") :face font-lock-builtin-face)
          (operator ,(nerd-icons-codicon "nf-cod-symbol_operator") :face font-lock-comment-delimiter-face)
          (param ,(nerd-icons-codicon "nf-cod-symbol_parameter") :face default)
          (property ,(nerd-icons-codicon "nf-cod-symbol_property") :face font-lock-variable-name-face)
          (reference ,(nerd-icons-codicon "nf-cod-references") :face font-lock-variable-name-face)
          (snippet ,(nerd-icons-codicon "nf-cod-symbol_snippet") :face font-lock-string-face)
          (string ,(nerd-icons-codicon "nf-cod-symbol_string") :face font-lock-string-face)
          (struct ,(nerd-icons-codicon "nf-cod-symbol_structure") :face font-lock-variable-name-face)
          (text ,(nerd-icons-codicon "nf-cod-text_size") :face font-lock-doc-face)
          (typeparameter ,(nerd-icons-codicon "nf-cod-list_unordered") :face font-lock-type-face)
          (type-parameter ,(nerd-icons-codicon "nf-cod-list_unordered") :face font-lock-type-face)
          (unit ,(nerd-icons-codicon "nf-cod-symbol_ruler") :face font-lock-constant-face)
          (value ,(nerd-icons-codicon "nf-cod-symbol_field") :face font-lock-builtin-face)
          (variable ,(nerd-icons-codicon "nf-cod-symbol_variable") :face font-lock-variable-name-face)
          (t ,(nerd-icons-codicon "nf-cod-code") :face font-lock-warning-face))))

;; Use awesome page break lines
(use-package page-break-lines
  :straight t
  :defer t
  :init
  (global-page-break-lines-mode))

;; Add color to brackets
(use-package rainbow-delimiters
  :straight t
  :defer t
  :hook (prog-mode . rainbow-delimiters-mode))

;; UI Customization
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)
(blink-cursor-mode -1)
;; nice scrolling
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)
(column-number-mode t) ; enable column number
(size-indication-mode t) ; show size on mode line

(use-package display-line-numbers
  :hook
  ((prog-mode text-mode conf-mode) . display-line-numbers-mode)
  :config
  (defcustom display-line-numbers-exempt-modes
    '(vterm-mode eshell-mode shell-mode term-mode ansi-term-mode
                 treemacs-mode dashboard-mode org-mode which-key-mode
  	       vterm-mode)
    "Major modes on which to disable line numbers."
    :group 'display-line-numbers
    :type 'list
    :version "green")

  (defun display-line-numbers--turn-on ()
    "Turn on line numbers except for certain major modes.
Exempt major modes are defined in `display-line-numbers-exempt-modes'."
    (unless (or (minibufferp)
                (member major-mode display-line-numbers-exempt-modes))
      (display-line-numbers-mode)))

  (setq display-line-numbers 't) ; Relative or Absolute
  ;; (global-hl-line-mode) ; Highlight current line
  )

;; Set up font
(add-to-list 'default-frame-alist
             '(font . "MonoLisa-11"))

(use-package treesit
  :config
  )

(use-package tree-sitter-module
  :after treesit
  :defer t
  :straight (tree-sitter-module
             :type git :host github
             :repo "casouri/tree-sitter-module"
             :pre-build (("./batch.sh"))
             :files ("dist/*.so" "dist/*.dll" "dist/*.dylib"))
  :init
  ;; Search for tree-sitter modules in this packages build directory.
  (with-eval-after-load 'treesit
    (add-to-list 'treesit-extra-load-path
                 (straight--build-dir "tree-sitter-module"))))

(use-package markdown-mode
  :straight t
  :mode ("READ\\.md\\'" . gfm-mode))

(use-package org
  :straight t
  :preface
  ;; Make most of the default modules opt-in to lighten its first-time load
  ;; delay. I sincerely doubt most users use them all.
  (defvar org-modules
    '(;; ol-w3m
      ;; ol-bbdb
      ol-bibtex
      ;; ol-docview
      ;; ol-gnus
      ;; ol-info
      ;; ol-irc
      ;; ol-mhe
      ;; ol-rmail
      ;; ol-eww
      ))
  :custom-face
  (org-level-1 ((t (:height 1.4))))
  (org-level-2 ((t (:height 1.3))))
  (org-level-3 ((t (:height 1.15))))
  :hook
  (org-mode . org-indent-mode)
  (org-mode . (lambda ()
                (toggle-truncate-lines nil)))
  :custom
  ;; Org files
  (org-directory "~/OneDrive/org/") ; Note directory
  (org-default-notes-file (concat org-directory "inbox.org")) ; Default entry point
  (org-agenda-files (list (concat org-directory "inbox.org")
                          (concat org-directory "todo.org")))

  ;; Useful settings
  (org-hide-leading-stars t)
  (org-startup-folded (quote overview)) ; Fold all by default
  (org-hide-emphasis-markers t) ; Hide emphasis markers
  (org-log-done 'time) ; Log time when finish a job
  (org-agenda-inhibit-startup t)
  (org-inhibit-startup t)
  (org-return-follows-link t) ; follow links when press RET
  (org-priority-faces '((?A :foreground "#BF616A")
                        (?B :foreground "#ebcb8b")
                        (?C :foreground "#81A1C1")))
  (org-todo-keywords
   '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
     (sequence "REPORT(r)" "BUG(b)" "KNOWNCAUSE(k)" "|" "FIXED(f)")
     (sequence "|" "CANCEL(c)")))
  (org-todo-keyword-faces
   '(("TODO" :foreground "#b48ead" :weight normal :underline t) ("NEXT" :foreground "#88c0d0" :weight normal :underline t) ("DONE" :foreground "#a3be8c" :weight normal :underline t)
     ("ISSUE" :foreground "#bf616a" :weight normal :underline t) ("FIXED" :foreground "#a3be8c" :weight normal :underline t)
     ("CANCEL" :foreground "#bf616a" :underline t)))
  (org-image-actual-width '(400))
  (org-reveal-root "https://revealjs.com"))

(use-package org-modern
  :straight t
  :hook
  (org-mode . org-modern-mode)
  (org-agenda-finalize . org-modern-agenda))

(use-package org-capture
  :defer t
  :config
  (setq org-capture-templates '(
                                ("t" "Todo" entry (file+headline "todo.org" "Task")
                                 "** TODO %?\n")
                                )))

(use-package org-roam
  :straight t
  :hook
  (org-load . org-roam-db-autosync-mode)
  :bind
  (("C-c n r" . org-roam-buffer-toggle)
   ("C-c n f" . org-roam-node-find)
   ("C-c n g" . 'org-roam-ui-open)
   ("C-c n i" . org-roam-node-insert)
   ("C-c n c" . org-roam-capture)
   ;; Dailies
   ("C-c n d d" . org-roam-dailies-capture-today)
   ("C-c n d t" . 'org-roam-dailies-find-today)
   ("C-c n d j" . 'org-roam-dailies-find-date))
  :custom
  (org-roam-directory "~/OneDrive/org/roam")
  (org-roam-dailies-directory "50 Journals")
  :config
  ;; Roam buffer now act as a side window
  (add-to-list 'display-buffer-alist
               '("\\*org-roam\\*"
                 (display-buffer-in-side-window)
                 (side . right)
                 (slot . 0)
                 (window-width . 0.28)
                 (window-parameters . ((no-other-window . t)
                                       (no-delete-other-windows . t)))))

  (setq org-roam-completion-everywhere t)
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
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
;; ;; Helper function by @real-or-random,
;; ;; retrieved from https://github.com/org-roam/org-roam/issues/507.
;; ;; This provide a minor mode that opens the org-roam-buffer when entering roam files.
(defun tim/org-roam-buffer-auto-toggle (frame)
  "Ensure that org-roam buffer is visible iff frame contains an org-roam file."
  (and (org-roam-file-p)
       (not (eq 'visible (org-roam-buffer--visibility)))
       (org-roam-buffer-toggle))

  ;; (with-selected-frame frame
  ;;   (when (xor
  ;;          (eq 'visible (org-roam-buffer--visibility))
  ;;          (seq-find
  ;;           (lambda (window) (org-roam-buffer-p (window-buffer window)))
  ;;           (window-list nil t nil)))
  ;;     (org-roam-buffer-toggle)))
  )

(define-minor-mode tim/org-roam-buffer-auto-mode
  "Global minor mode for toggling the org-roam buffer automatically.

When this global minor mode is enabled, then the org-roam backlink buffer is
shown if and only if the current frame has a window with an org-roam file."
  :global t
  :lighter " OrgRoamBuf"
  (if tim/org-roam-buffer-auto-mode
      (add-hook 'window-buffer-change-functions 'tim/org-roam-buffer-auto-toggle)
    (remove-hook 'window-buffer-change-functions 'tim/org-roam-buffer-auto-toggle)))

;; (tim/org-roam-buffer-auto-mode t)

(use-package websocket
  :straight t
  :after org-roam)

(use-package org-roam-ui
  :straight
  (:host github :repo "org-roam/org-roam-ui" :branch "main" :files ("*.el" "out"))
  ;; :hook
  ;; (org-roam-mode . org-roam-ui-mode)
  :after org-roam
  ;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
  ;;         a hookable mode anymore, you're advised to pick something yourself
  ;;         if you don't care about startup time, use
  ;;  :hook (after-init . org-roam-ui-mode)
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))

(use-package org-roam-protocol
  :after org-roam)

(use-package org-appear
  :straight (org-appear
  	   :type git
  	   :host github
  	   :repo "awth13/org-appear")
  :hook
  (org-mode . org-appear-mode)
  (org-mode . (lambda ()
  	      (add-hook 'meow-insert-enter-hook
  			#'org-appear-manual-start
  			nil
  			t)
  	      (add-hook 'meow-insert-exit-hook
  			#'org-appear-manual-stop
  			nil
  			t)))  		      
  :custom
  (org-appear-autolinks t)
  (org-appear-trigger 'manual))

(use-package rustic
  :straight t
  :mode ("\\.rs$" . rustic-mode)
  :custom
  (rustic-analyzer-command '("rustup" "run" "stable" "rust-analyzer"))
  :config
  (setq rustic-lsp-client 'lsp)
  (setq rustic-indent-method-chain t)
  (setq rust-prettify-symbols-alist nil)
  ;;(setq rustic-format-trigger 'on-save)
  (setq rustic-format-on-save t)
  )

;; (use-package treesit
;;   :mode ("\\.rs$" . rust-ts-mode)
;;   :hook
;;   (rust-ts-mode . eglot-ensure)
;;   :config
;;   )

(add-to-list 'major-mode-remap-alist
  	   '(conf-toml-mode . toml-ts-mode))

(add-to-list 'auto-mode-alist
  	   '("\\.ya?ml$" . yaml-ts-mode))

(defun my/exwm-startup-applications ()
  "Some applications that runs at startup."
  (async-start-process "polybar" "/usr/bin/bash" nil "-c $HOME/.config/polybar/launch.sh dark &")
  )
(use-package exwm
  :disabled t
  :straight t
  :config
  (setq exwm-workspace-minibuffer-position nil)

  (my/exwm-startup-applications)
  ;; (server-start)
  )

(use-package exwm-config
  :after exwm
  :config
  (defun my/exwm-update-title ()
    (pcase exwm-class-name
      ("qutebrowser" (exwm-workspace-rename-buffer (format "Qutebrowser: %s" exwm-title)))))

  (defun my/configure-window-by-class ()
    (interactive)
    (message "Window '%s' appeared!" exwm-class-name)
    ;; Send programs to specific workspaces
    (pcase exwm-class-name
      ("qutebrowser"
       (exwm-workspace-move-window 1)
       (exwm-layout-toggle-mode-line)))
    )

  ;; Exclude EXWM buffers
  (defun my/window-exwm-p (window)
    "Return t if WINDOW is exwm-mode"
    (equal 'exwm-mode (buffer-local-value 'major-mode (window-buffer window))))

  ;; discourage the use of exwm-windows by `display-buffer'
  (defun my/window-list-filter-advice (result)
    "Advice fn to exclude exwm windows from returned list, unless all are exwm."
    (or (-reject 'my/window-exwm-p result) result))

  (defun my/display-buffer-around-advice (orig-fun buffer-or-name
  						 &optional action frame)
    "Advice for `display-buffer' to only use non-exwm windows if possible."
    (advice-add #'window-list-1 :filter-return #'my/window-list-filter-advice)
    (unwind-protect
      (apply orig-fun buffer-or-name action frame)
      (advice-remove #'window-list-1 #'my/window-list-filter-advice)))

  (advice-add #'display-buffer :around #'my/display-buffer-around-advice)

  (add-hook 'exwm-manage-finish-hook #'my/configure-window-by-class)
  ;; Make class name the buffer name
  (add-hook 'exwm-update-class-hook #'(lambda ()
  				      (exwm-workspace-rename-buffer exwm-class-name)))
  ;; Use window title as buffer name
  (add-hook 'exwm-update-title-hook #'my/exwm-update-title)


  ;; Set the initial workspace number.
  (unless (get 'exwm-workspace-number 'saved-value)
    (setq exwm-workspace-number 4))
  ;; Global keybindings.
  (unless (get 'exwm-input-global-keys 'saved-value)
    (setq exwm-input-global-keys
          `(
            ;; 's-r': Reset (to line-mode).
            (,(kbd "s-r") . exwm-reset)
            ;; 's-w': Switch workspace.
            (,(kbd "s-w") . exwm-workspace-switch)
  	  ;; 's-\'': Swap workspaces.
  	  (,(kbd "s-'") . exwm-workspace-swap)
  	  ;; 's-b': Switch to buffer workspace.
  	  (,(kbd "s-b") . exwm-workspace-switch-to-buffer)
            ;; 's-&': Launch application.
            (,(kbd "s-D") . (lambda (command)
  			    (interactive (list (read-shell-command "$ ")))
  			    (start-process-shell-command command nil command)))
            ;; 's-N': Switch to certain workspace.
            ,@(mapcar (lambda (i)
                        `(,(kbd (format "s-%d" i)) .
                          (lambda ()
                            (interactive)
                            (exwm-workspace-switch-create ,i))))
  		    (number-sequence 0 4))
  	  ,@(cl-mapcar (lambda (c n)
                           `(,(kbd (format "s-%c" c)) .
                             (lambda ()
  			     (interactive)
  			     (exwm-workspace-move-window ,n)
  			     (exwm-workspace-switch ,n))))
                         '(?\) ?! ?@ ?# ?$)
                         ;; '(?\= ?! ?\" ?# ?¤ ?% ?& ?/ ?\( ?\))
                         (number-sequence 0 9))
  	  (,(kbd "s-h") . windmove-left)  ;; Move to window to the left of current one. Uses universal arg
            (,(kbd "s-j") . windmove-down)  ;; Move to window below current one. Uses universal arg
            (,(kbd "s-k") . windmove-up)    ;; Move to window above current one. Uses universal arg
            (,(kbd "s-l") . windmove-right) ;; Move to window to the right of current one. Uses universal arg
  	  (,(kbd "s-C") . kill-this-buffer))))
  ;; Line-editing shortcuts
  (unless (get 'exwm-input-simulation-keys 'saved-value)
    (setq exwm-input-simulation-keys
          '(([?\C-b] . [left])
            ([?\C-f] . [right])
            ([?\C-p] . [up])
            ([?\C-n] . [down])
            ([?\C-a] . [home])
            ([?\C-e] . [end])
            ([?\M-v] . [prior])
            ([?\C-v] . [next])
            ([?\C-d] . [delete])
            ([?\C-k] . [S-end delete]))))

  (setq exwm-input-prefix-keys
      '(?\C-x
  	?\C-u
  	?\C-h
  	?\M-x
  	?\M-`
  	?\M-&
  	?\M-i ;; emacs-rime force Chinese IM
  	?\s-q
  	?\s-f
  	?\M-:
  	?\C-\M-j  ;; Buffer list
  	?\C-\\ ))

  ;; Enable EXWM
  (exwm-enable)
  ;; Configure Ido
  ;; (exwm-config-ido)
  ;; Other configurations
  (exwm-config-misc)
  )

(use-package exwm-xim
  :after exwm
  :config
  (exwm-xim-enable)
  (push ?\C-\\ exwm-input-prefix-keys))

(use-package exwm-randr
  :after exwm
  :config
  (setq exwm-randr-workspace-monitor-plist '(0 "eDP1" 1 "DP3"))
  (add-hook 'exwm-randr-screen-change-hook
            (lambda ()
              (start-process-shell-command
               "xrandr" nil
  	     "xrandr --output eDP1 --primary --mode 3840x2160 --pos 2160x1680 --rotate normal --output DP1 --off --output DP2 --off --output DP3 --mode 1920x1080 --pos 0x0 --rotate left --scale 2.0x2.0 --output VIRTUAL1 --off")))
  (exwm-randr-enable)
  )

(use-package exwm-systemtray
  :after exwm
  :config
  (exwm-systemtray-enable))

(use-package exwm-edit
  :straight t
  :after exwm
  :config
  (defun my/on-exwm-edit-compose ()
    (funcall 'markdown-mode))
  (add-hook 'exwm-edit-compose-hook 'my/on-exwm-edit-compose))