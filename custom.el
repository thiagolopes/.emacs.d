(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(amx-mode t)
 '(backup-by-copying t)
 '(before-save-hook '(whitespace-cleanup))
 '(blink-cursor-blinks 0)
 '(blink-cursor-mode t)
 '(c-basic-offset 8)
 '(c-ts-mode-indent-offset 8)
 '(company-format-margin-function nil)
 '(company-frontends
   '(company-pseudo-tooltip-frontend company-echo-metadata-frontend))
 '(completion-styles '(hotfuzz basic))
 '(cursor-type 'bar)
 '(custom-buffer-indent 4)
 '(custom-buffer-sort-alphabetically t)
 '(custom-enabled-themes '(modus-vivendi))
 '(custom-safe-themes
   '("20692ee369ff5ad4ccdf912d6f44f87ca6ec45ee67a607bcb256a86867348b6d"
     "77f281064ea1c8b14938866e21c4e51e4168e05db98863bd7430f1352cab294a"
     "41cf93de3b55f223a55e84eae054ef24269a17d374e8446d4f9113c4cb3f3d1a"
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
 '(global-so-long-mode t)
 '(hledger-currency-string "R$")
 '(ibuffer-expert t)
 '(indent-tabs-mode nil)
 '(indicate-buffer-boundaries 'left)
 '(indicate-empty-lines t)
 '(inhibit-startup-screen t)
 '(line-spacing 0.4)
 '(marginalia-align 'center)
 '(mode-line-position '("@%l:%c ") t)
 '(modus-themes-bold-constructs t)
 '(modus-themes-common-palette-overrides
   '((cursor "white") (builtin magenta) (comment yellow-faint)
     (constant magenta-cooler) (docstring green-faint)
     (docmarkup magenta-faint) (fnname magenta-warmer) (keyword cyan)
     (preprocessor cyan-cooler) (string green-cooler)
     (type magenta-cooler) (variable blue-warmer)
     (rx-construct magenta-warmer) (rx-backslash blue-cooler)
     (bg-paren-match unspecified) (fg-paren-match red-intense)
     (fg-prompt cyan) (bg-prompt bg-cyan-nuanced)
     (bg-mode-line-active bg-lavender) (fg-mode-line-active fg-main)
     (border-mode-line-active "white")
     (fg-mode-line-active fg-main) (bg-hover bg-yellow-intense)
     (underline-err yellow-intense)
     (underline-warning magenta-intense)
     (underline-note green-intense) (fg-line-number-inactive "gray50")
     (fg-line-number-active red-cooler)
     (bg-line-number-inactive unspecified)
     (bg-line-number-active unspecified) (fg-added blue)
     (fg-added-intense blue-intense) (fg-changed magenta-cooler)
     (fg-changed-intense magenta-intense) (fg-removed yellow-warmer)
     (fg-removed-intense yellow-intense)))
 '(modus-themes-completions '((matches bold italic)))
 '(modus-themes-italic-constructs nil)
 '(modus-themes-mixed-fonts t)
 '(modus-themes-prompts '(italic bold))
 '(modus-themes-variable-pitch-ui t)
 '(package-selected-packages
   '(amx buffer-name-relative cmake-mode company company-posframe consult
         corfu ctrlf diminish dockerfile-mode dumb-jump expand-region
         expreg fancy-compilation git-link git-timemachine
         goto-last-change goto-line-preview helpful highlight-numbers
         hledger-mode hotfuzz hungry-delete kind-icon lua-mode magit
         marginalia markdown-mode mode-line-bell modus-themes
         multiple-cursors mwim no-littering org-modern popwin
         rainbow-delimiters rainbow-mode rust-mode sudo-edit
         telephone-line treemacs treesit-auto undo-fu undo-fu-session
         uuidgen vertico virtualenvwrapper visual-fill-column vundo
         yaml-mode zenburn-theme))
 '(prog-mode-hook '(display-line-numbers-mode))
 '(project-mode-line t)
 '(read-buffer-completion-ignore-case t)
 '(read-file-name-completion-ignore-case t)
 '(recentf-mode t)
 '(require-final-newline t)
 '(scroll-conservatively 101)
 '(scroll-step 1)
 '(transient-mark-mode nil)
 '(treemacs-fringe-indicator-mode 'only-when-focused nil nil "Customized with use-package treemacs")
 '(undo-fu-session-global-mode t)
 '(undo-limit 67108864)
 '(undo-strong-limit 100663296)
 '(use-dialog-box nil)
 '(use-short-answers t)
 '(user-mail-address "thiagolopes@protonmail.com")
 '(visual-line-fringe-indicators '(nil nil))
 '(whitespace-style
   '(face spaces empty tabs newline trailing space-mark tab-mark))
 '(x-underline-at-descent-line t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:height 120 :family "FiraCode"))))
 '(corfu-default ((t (:inherit default))))
 '(magit-diff-lines-boundary ((t (:extend t :background "gray50"))))
 '(mc/cursor-bar-face ((t (:background "chartreuse" :foreground "#ffffff" :height 1))))
 '(modus-themes-ui-variable-pitch ((t (:inherit variable-pitch))) t)
 '(region ((t :extend nil)))
 '(variable-pitch ((t (:family "Adwaita Sans")))))
