(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(amx-mode t)
 '(backup-by-copying t)
 '(c-basic-offset 8)
 '(company-format-margin-function nil)
 '(company-frontends
   '(company-pseudo-tooltip-frontend company-echo-metadata-frontend))
 '(completion-styles '(flex partial-completion substring basic))
 '(ctrlf-mode t)
 '(custom-buffer-indent 4)
 '(custom-buffer-sort-alphabetically t)
 '(custom-enabled-themes '(borland-blue))
 '(custom-safe-themes
   '("e72e267b6037f0b99804a3b9167675147841a7b64798ca1d9db8ee4c9bd22e75"
     default))
 '(dabbrev-case-replace nil)
 '(dired-listing-switches "-alh")
 '(eglot-autoshutdown t)
 '(eglot-extend-to-xref t)
 '(eglot-ignored-server-capabilities
   '(:documentHighlightProvider :codeLensProvider
                                :documentFormattingProvider
                                :documentOnTypeFormattingProvider
                                :foldingRangeProvider))
 '(eglot-menu-string "lsp")
 '(fancy-compilation-mode t)
 '(fringe-mode 10 nil (fringe))
 '(global-auto-revert-mode t)
 '(global-corfu-mode t)
 '(global-hl-line-mode t)
 '(hledger-currency-string "R$")
 '(ibuffer-expert t)
 '(indent-tabs-mode nil)
 '(indicate-buffer-boundaries 'left)
 '(indicate-empty-lines t)
 '(inhibit-startup-screen t)
 '(package-selected-packages
   '(amx cmake-mode company company-posframe consult corfu ctrlf diminish
         dockerfile-mode dumb-jump expand-region fancy-compilation
         git-link git-timemachine goto-last-change goto-line-preview
         helpful highlight-numbers hledger-mode hungry-delete lua-mode
         magit marginalia markdown-mode mode-line-bell
         multiple-cursors mwim no-littering orderless org-modern
         popwin rainbow-delimiters rainbow-mode rust-mode solaire-mode
         sudo-edit treemacs undo-fu undo-fu-session uuidgen vertico
         virtualenvwrapper vundo yaml-mode zenburn-theme))
 '(read-buffer-completion-ignore-case t)
 '(read-file-name-completion-ignore-case t)
 '(require-final-newline t)
 '(scroll-margin 1)
 '(scroll-preserve-screen-position t)
 '(scroll-step 1)
 '(solaire-global-mode t)
 '(transient-mark-mode nil)
 '(undo-fu-session-global-mode t)
 '(undo-limit 67108864)
 '(undo-strong-limit 100663296)
 '(use-dialog-box nil)
 '(use-short-answers t)
 '(user-mail-address "thiagolopes@protonmail.com")
 '(visual-line-fringe-indicators '(nil nil))
 '(whitespace-style
   '(face spaces empty tabs newline trailing space-mark tab-mark))
 '(x-underline-at-descent-line t)
 '(xref-show-definitions-function 'consult-xref)
 '(xref-show-xrefs-function 'consult-xref))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:height 120 :family "Iosevka")))))
