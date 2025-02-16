(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(backup-by-copying t)
 '(c-basic-offset 8)
 '(company-format-margin-function nil)
 '(company-frontends
   '(company-pseudo-tooltip-frontend company-echo-metadata-frontend))
 '(completion-styles '(flex partial-completion substring basic))
 '(ctrlf-mode t)
 '(custom-buffer-indent 4)
 '(custom-buffer-sort-alphabetically t)
 '(custom-enabled-themes '(cobalt))
 '(custom-safe-themes
   '("c230e79b42f11bf7526618c12f3e1cd1a4d669dab4c70e22ad90eb445f67d9b0" default))
 '(dired-listing-switches "-alh")
 '(eglot-autoshutdown t)
 '(eglot-extend-to-xref t)
 '(eglot-ignored-server-capabilities
   '(:documentHighlightProvider :codeLensProvider :documentFormattingProvider :documentOnTypeFormattingProvider :foldingRangeProvider))
 '(eglot-menu-string "lsp")
 '(fancy-compilation-mode t)
 '(fringe-mode 10 nil (fringe))
 '(global-auto-revert-mode t)
 '(global-hl-line-mode t)
 '(hledger-currency-string "R$")
 '(ibuffer-expert t)
 '(indent-tabs-mode nil)
 '(indicate-buffer-boundaries 'left)
 '(indicate-empty-lines t)
 '(inhibit-startup-screen t)
 '(package-selected-packages
   '(marginalia vertico helpful popwin cmake-mode highlight-numbers git-timemachine sudo-edit git-link company-posframe rainbow-mode hledger-mode mwim expand-region hungry-delete uuidgen multiple-cursors fancy-compilation goto-last-change goto-line-preview diminish markdown-mode no-littering mode-line-bell company virtualenvwrapper dumb-jump consult solaire-mode ctrlf dockerfile-mode lua-mode rust-mode yaml-mode undo-fu undo-fu-session vundo rainbow-delimiters org-modern magit zenburn-theme))
 '(read-buffer-completion-ignore-case t)
 '(read-file-name-completion-ignore-case t)
 '(require-final-newline t)
 '(scroll-margin 1)
 '(scroll-preserve-screen-position t)
 '(scroll-step 1)
 '(solaire-global-mode t)
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
 '(default ((t (:height 160 :family "Iosevka")))))
