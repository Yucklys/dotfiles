;; Reduce number of gc at startup
(add-hook 'emacs-startup-hook
  (lambda ()
    (setq gc-cons-threshold 16777216 ; 16mb
          gc-cons-percentage 0.1)))

;; in ~/.emacs.d/init.el (or ~/.emacs.d/early-init.el in Emacs 27)
(setq package-enable-at-startup nil ; don't auto-initialize!
      ;; don't add that `custom-set-variables' block to my init.el!
      package--init-file-ensured t)

;;; Native compilation support (see http://akrl.sdf.org/gccemacs.html)
(when (boundp 'native-comp-eln-load-path)
  ;; Don't store eln files in ~/.emacs.d/eln-cache
  ;; Use startup-redirect-eln-cache in Emacs 29
  (startup-redirect-eln-cache (expand-file-name (convert-standard-filename "var/eln-cache/")
                                                user-emacs-directory)))

(setq package-enable-at-startup nil) ; For Emacs version >= 27
(setq inhibit-automatic-native-compilation t)

;; Integration of straight.el and flycheck.
(setq straight-fix-flycheck t)

;; Disable native-comp warning buffer.
(setq native-comp-async-report-warnings-errors nil)
