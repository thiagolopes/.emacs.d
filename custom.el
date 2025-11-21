(message "[config] starting custom.el")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(backup-by-copying t)
 '(before-save-hook '(whitespace-cleanup))
 '(blink-cursor-blinks 0)
 '(blink-cursor-mode t)
 '(c-basic-offset 4)
 '(c-ts-mode-indent-offset 4)
 '(column-number-mode t)
 ;; '(compilation-auto-jump-to-first-error t)
 '(compilation-max-output-line-length nil)
 '(compilation-scroll-output t)
 '(completion-auto-help nil)
 '(context-menu-mode t)
 '(cursor-type 'box)
 '(custom-buffer-indent 4)
 '(custom-buffer-sort-alphabetically t)
 '(custom-enabled-themes '(mini-gruber-darker))
 '(custom-safe-themes
   '("9111b904c469caf20ad26f870c689e46c444c081204cf236fc9dc4be49772f06"
     default))
 '(dabbrev-case-replace nil)
 '(delete-selection-mode t)
 '(dired-listing-switches "-alh")
 '(display-line-numbers-width 4)
 '(eglot-autoshutdown t)
 '(eglot-extend-to-xref t)
 '(eglot-ignored-server-capabilities
   '(:codeLensProvider :documentOnTypeFormattingProvider
                       :foldingRangeProvider))
 '(global-auto-revert-mode t)
 '(global-corfu-mode t)
 '(global-so-long-mode t)
 '(global-visual-line-mode nil)
 '(hledger-currency-string "R$")
 '(ibuffer-expert t)
 '(indent-tabs-mode nil)
 '(indicate-buffer-boundaries 'left)
 '(indicate-empty-lines t)
 '(inhibit-startup-screen t)
 '(kill-ring-max 400)
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
 '(modus-themes-prompts '(bold))
 '(modus-themes-variable-pitch-ui t)
 '(package-selected-packages
   '(anzu buffer-name-relative cape cmake-mode consult corfu-terminal
          diminish dockerfile-mode dumb-jump ef-themes eglot embark
          embark-consult exec-path-from-shell expand-region expreg
          flycheck flyover git-link git-timemachine goto-last-change
          goto-line-preview gruber-darker-theme guess-language helpful
          highlight-numbers hledger-mode hotfuzz hungry-delete jinx
          magit marginalia markdown-mode mode-line-bell modus-themes
          move-dup multiple-cursors mwim nerd-icons-completion
          nerd-icons-corfu nerd-icons-dired nerd-icons-ibuffer
          no-littering nvm ob-mongo org-appear org-modern
          page-break-lines popwin rainbow-delimiters rainbow-mode rg
          sudo-edit super-save treemacs treemacs-nerd-icons
          typescript-mode undo-fu undo-fu-session uuidgen verb
          visual-fill-column vundo web-mode yafolding zig-mode))
 '(pixel-scroll-precision-mode t)
 '(prog-mode-hook '(display-line-numbers-mode))
 '(project-mode-line t)
 '(read-buffer-completion-ignore-case t)
 '(read-file-name-completion-ignore-case t)
 '(repeat-mode t)
 '(require-final-newline t)
 '(safe-local-variable-values
   '((eval when (fboundp 'rainbow-mode) (rainbow-mode 1))
     (checkdoc-minor-mode . t)))
 '(show-paren-mode t)
 '(transient-mark-mode nil)
 '(undo-fu-session-global-mode t)
 '(undo-limit 67108864)
 '(undo-strong-limit 100663296)
 '(use-dialog-box nil)
 '(use-short-answers t)
 '(user-mail-address "thiagolopes@protonmail.com")
 '(visual-line-fringe-indicators '(nil nil))
 '(which-key-idle-delay 1.5)
 '(which-key-mode t)
 '(which-key-show-early-on-C-h t)
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
 '(default ((t (:height 160 :family "Iosevka"))))
 '(modus-themes-ui-variable-pitch ((t (:inherit variable-pitch))) t)
 '(region ((t :extend nil)))
 '(variable-pitch ((t (:family "Roboto")))))

(message "[config] finisehd custom.el")
