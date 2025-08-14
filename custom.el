(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(amx-mode t)
 '(backup-by-copying t)
 '(beacon-blink-when-focused t)
 '(beacon-color 0.2)
 '(beacon-mode t)
 '(before-save-hook '(whitespace-cleanup))
 '(blink-cursor-blinks 0)
 '(blink-cursor-mode t)
 '(c-basic-offset 4)
 '(c-ts-mode-indent-offset 4)
 '(column-number-mode t)
 '(company-format-margin-function nil)
 '(company-frontends
   '(company-pseudo-tooltip-frontend company-echo-metadata-frontend))
 '(completion-styles '(hotfuzz basic))
 '(context-menu-mode t)
 '(cursor-type 'bar)
 '(custom-buffer-indent 4)
 '(custom-buffer-sort-alphabetically t)
 '(custom-enabled-themes '(cobalt))
 '(custom-safe-themes
   '("c5ffe3ad8e6cc68a12808529dc89927941a86618c8d8817613c9fa1b5cb707f5"
     default))
 '(dabbrev-case-replace nil)
 '(delete-selection-mode t)
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
 '(global-visual-line-mode t)
 '(hledger-currency-string "R$")
 '(ibuffer-expert t)
 '(indent-tabs-mode nil)
 '(indicate-buffer-boundaries 'left)
 '(indicate-empty-lines t)
 '(inhibit-startup-screen t)
 '(line-spacing 0.2)
 '(marginalia-align 'center)
 '(mode-line-position '("@%l:%c ") t)
 '(modus-themes-bold-constructs nil)
 '(modus-themes-common-palette-overrides
   '((cursor magenta-warmer) (builtin magenta) (comment yellow-faint)
     (constant magenta-cooler) (docstring green-faint)
     (docmarkup magenta-faint) (fnname magenta-warmer) (keyword cyan)
     (preprocessor cyan-cooler) (string green-cooler)
     (type magenta-cooler) (variable blue-warmer)
     (rx-construct magenta-warmer) (rx-backslash blue-cooler)
     (bg-paren-match unspecified) (fg-paren-match red-intense)
     (fg-prompt cyan) (bg-prompt bg-cyan-nuanced)
     (bg-mode-line-active bg-lavender) (fg-mode-line-active fg-main)
     (border-mode-line-active fg-lavender)
     (fg-mode-line-active fg-main) (bg-hover bg-yellow-intense)
     (underline-err yellow-intense)
     (underline-warning magenta-intense)
     (underline-note green-intense) (fg-line-number-inactive fg-dim)
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
   '(amx beacon buffer-name-relative cmake-mode company company-posframe
         consult corfu ctrlf diminish dockerfile-mode dumb-jump
         exec-path-from-shell expand-region expreg fancy-compilation
         git-link git-timemachine goto-last-change goto-line-preview
         helpful highlight-numbers hledger-mode hotfuzz hungry-delete
         kind-icon lua-mode magit marginalia markdown-mode
         mode-line-bell modus-themes multiple-cursors mwim
         no-littering org-modern popwin rainbow-delimiters
         rainbow-mode rust-mode sudo-edit telephone-line treemacs
         treesit-auto undo-fu undo-fu-session uuidgen vertico
         virtualenvwrapper visual-fill-column vundo yaml-mode
         zenburn-theme))
 '(prog-mode-hook '(display-line-numbers-mode))
 '(project-mode-line t)
 '(read-buffer-completion-ignore-case t)
 '(read-file-name-completion-ignore-case t)
 '(recentf-mode t)
 '(repeat-mode t)
 '(require-final-newline t)
 '(save-place-mode t)
 '(savehist-mode t)
 '(scroll-conservatively 101)
 '(scroll-step 1)
 '(show-paren-mode t)
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
 '(windmove-default-keybindings t)
 '(winner-mode t)
 '(x-underline-at-descent-line t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:height 130 :family "Roboto Mono"))))
 '(line-number ((t (:inherit default :height 0.8))))
 '(corfu-default ((t (:inherit default))))
 '(magit-diff-lines-boundary ((t (:extend t :background "gray50"))))
 '(mc/cursor-bar-face ((t (:background "chartreuse" :foreground "#ffffff" :height 1))))
 '(modus-themes-ui-variable-pitch ((t (:inherit variable-pitch))) t)
 '(region ((t :extend nil)))
 '(variable-pitch ((t (:family "Roboto")))))
