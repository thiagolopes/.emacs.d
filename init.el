;;; init.el -- init file only with use-package  -*- lexical-binding: t; -*-
;;; Commentary:
;;;  this package will run after early-init.el
;;; Code:
(use-package cmake-mode)
(use-package better-defaults)
(use-package git-timemachine)
(use-package git-link)
(use-package sudo-edit)
(use-package irony)
(use-package pdf-tools :defer 5)
(use-package web-mode :defer 5)
(use-package transpose-frame :defer 3)
(use-package i3wm-config-mode :defer 2)
(use-package ivy)
(use-package virtualenvwrapper)

(use-package page-break-lines
  :config
  (global-page-break-lines-mode))

(use-package minions
  :init
  (minions-mode))

(use-package shell-pop
  :config
  (custom-set-variables
   '(shell-pop-shell-type (quote ("ansi-term" "*ansi-term*" (lambda nil (ansi-term shell-pop-term-shell))))))
  (general-define-key "<f12>" #'shell-pop))

(use-package corfu
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-auto-prefix 3)
  (corfu-auto-delay 0.0)
  (corfu-quit-at-boundary 't)
  (corfu-echo-documetation 0.25)
  (corfu-preview-current 'insert)
  (corfu-preselect-first nil)
  :bind
  (:map corfu-map
        ("M-SPC" . corfu-insert-separator)
        ("RET" . nil)) ;; let my enter alone please
  :init
  (global-set-key (kbd "C-<tab>") 'completion-at-point)
  (corfu-echo-mode t)
  (corfu-popupinfo-mode t)
  (global-corfu-mode t))

(use-package corfu-terminal
  :init
  (unless (display-graphic-p)
    (corfu-terminal-mode +1)))

(use-package cape
  :defer 1
  :init
  ;; (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block)
  (add-to-list 'completion-at-point-functions #'cape-history)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  (add-to-list 'completion-at-point-functions #'cape-abbrev)
  :config
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-silent)
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-purify))

(use-package kind-icon
    :defer 1
    :config
    (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package no-littering
  :config
  (setq auto-save-file-name-transforms
        `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))))

(use-package which-key
  :defer 2
  :custom
  (which-key-allow-evil-operators t)
  (which-key-show-remaining-keys t)
  (which-key-sort-order 'which-key-prefix-then-key-order)
  :config
  (which-key-mode t)
  (which-key-setup-minibuffer)
  (set-face-attribute
   'which-key-local-map-description-face nil :weight 'bold))

(use-package undo-tree
  :init
  (global-undo-tree-mode)
  :config
  (setq undo-tree-visualizer-diff t
        undo-tree-auto-save-history t
        undo-tree-history-directory-alist `(("." . ,(concat user-emacs-directory "cache/fu/undo-tree-hist/")))
        undo-tree-enable-undo-in-region t
        ;; Increase undo limits to avoid emacs prematurely truncating the undo
        ;; history and corrupting the tree. This is larger than the undo-fu
        ;; defaults because undo-tree trees consume exponentially more space,
        ;; and then some when `undo-tree-enable-undo-in-region' is involved. See
        ;; syl20bnr/spacemacs#12110
        undo-limit 800000            ; 800kb (default is 160kb)
        undo-strong-limit 12000000   ; 12mb  (default is 240kb)
        undo-outer-limit 128000000)) ; 128mb (default is 24mb))

(use-package unicode-fonts
  :after persistent-soft
  :config
  (unicode-fonts-setup))

(use-package mixed-pitch
  :commands mixed-pitch-mode
  :custom
  (mixed-pitch-set-height t))

;; A more complex, more lazy-loaded config
(use-package solaire-mode
  :defer 5
  :hook
  ;; Ensure solaire-mode is running in all solaire-mode buffers
  (change-major-mode . turn-on-solaire-mode)
  ;; ...if you use auto-revert-mode, this prevents solaire-mode from turning
  ;; itself off every time Emacs reverts the file
  (after-revert . turn-on-solaire-mode)
  ;; To enable solaire-mode unconditionally for certain modes:
  (ediff-prepare-buffer . solaire-mode)
  :custom
  (solaire-mode-auto-swap-bg t)
  :config
  (solaire-global-mode +1))

(use-package info-colors
  :config
  (add-hook 'Info-selection-hook 'info-colors-fontify-node))

(use-package orderless
  :custom
  (completion-styles '(orderless))
  :config
  (defun prefix-if-tilde (pattern _index _total)
    (when (string-suffix-p "~" pattern)
      `(orderless-prefixes . ,(substring pattern 0 -1))))

  (defun regexp-if-slash (pattern _index _total)
    (when (string-prefix-p "/" pattern)
      `(orderless-regexp . ,(substring pattern 1))))

  (defun literal-if-equal (pattern _index _total)
    (when (string-suffix-p "=" pattern)
      `(orderless-literal . ,(substring pattern 0 -1))))

  (defun without-if-bang (pattern _index _total)
    (cond
     ((equal "!" pattern)
      '(orderless-literal . ""))
     ((string-prefix-p "!" pattern)
      `(orderless-without-literal . ,(substring pattern 1)))))

  (setq orderless-matching-styles '(orderless-flex))
  (setq orderless-style-dispatchers
        '(prefix-if-tilde
          regexp-if-slash
          literal-if-equal
          without-if-bang)))

(use-package expand-region
  :bind ("M-@" . er/expand-region))

(use-package smartparens
  :defer 5
  :bind
  ("C-)" . sp-forward-slurp-sexp)
  ("C-(" . sp-forward-barf-sexp))

(use-package cider
  :defer 2
  :config
  (setq-default nrepl-hide-special-buffers t
                nrepl-log-messages nil
                cider-font-lock-dynamically '(macro core function var deprecated)
                cider-overlays-use-font-lock t
                cider-prompt-for-symbol nil
                cider-inject-dependencies-at-jack-in nil
                cider-repl-history-display-duplicates nil
                cider-repl-history-display-style 'one-line
                cider-repl-history-file (concat user-emacs-directory "cache/cider-repl-history")
                cider-repl-history-highlight-current-entry t
                cider-repl-history-quit-action 'delete-and-restore
                cider-repl-history-highlight-inserted-item t
                cider-repl-history-size 1000
                cider-repl-result-prefix ";; => "
                cider-repl-print-length 100
                cider-repl-use-clojure-font-lock t
                cider-repl-use-pretty-printing t
                cider-repl-wrap-history nil
                cider-stacktrace-default-filters '(tooling dup)
                cider-repl-display-help-banner nil
                cider-repl-pop-to-buffer-on-connect 'display-only))

(use-package hl-todo
  :hook (prog-mode . hl-todo-mode)
  :hook (yaml-mode . hl-todo-mode)
  :config
  (setq hl-todo-highlight-punctuation ":"
        hl-todo-keyword-faces
        '(("TODO" warning bold)
          ("FIXME" error bold)
          ("REVIEW" font-lock-keyword-face bold)
          ("HACK" font-lock-constant-face bold)
          ("DEPRECATED" font-lock-doc-face bold)
          ("NOTE" success bold)
          ("BUG" error bold)
          ("XXX" font-lock-constant-face bold))))

(use-package multiple-cursors
  :defer 2
  :config
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this))

(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

(use-package projectile
  :init
  (setq projectile-project-search-path '(("~/dev/" . 1) ("~/.emacs.d/" . 1)))
  (setq projectile-globally-ignored-files '(".DS_Store" "TAGS")
        projectile-globally-ignored-file-suffixes '(".elc" ".pyc" ".o")
        projectile-kill-buffers-filter 'kill-only-files)

  (global-set-key [remap evil-jump-to-tag] #'projectile-find-tag)
  (global-set-key [remap find-tag]         #'projectile-find-tag))

(use-package winner
  :preface (defvar winner-dont-bind-my-keys t) ; I'll bind keys myself
  :hook (dashboard-setup-startup-hook . winner-mode)
  :config
  (winner-mode 1)
  (appendq! winner-boring-buffers
            '("*Compile-Log*" "*inferior-lisp*" "*Fuzzy Completions*"
              "*Apropos*" "*Help*" "*cvs*" "*Buffer List*" "*Ibuffer*"
              "*esh command on file*")))

(use-package paren
  :defer 2
  :hook (dashboard-setup-startup-hook . show-paren-mode)
  :config
  (setq show-paren-delay 0.1
        show-paren-highlight-openparen t
        show-paren-when-point-inside-paren t
        show-paren-when-point-in-periphery t))

(use-package rainbow-delimiters
  :bind
  ("<f8>" . rainbow-delimiters-mode)
  :config
  ;; (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
  (setq rainbow-delimiters-max-face-count 4))

(use-package undo-fu-session
  :custom
  (undo-fu-session-directory (concat user-emacs-directory "cache/fu/undo-fu-session/"))
  :config
  (undo-fu-session-global-mode t)
  (setq undo-fu-session-incompatible-files '("\\.gpg$" "/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'"))
  (when (executable-find "zstd")
    ;; There are other algorithms available, but zstd is the fastest, and speed
    ;; is our priority within Emacs
    (setq undo-fu-session-compression 'zst)))

(use-package ibuffer-projectile
  ;; Group ibuffer's list by project root
  :hook (ibuffer . ibuffer-projectile-set-filter-groups))

(use-package magit
  :init
  (setq magit-auto-revert-mode nil)  ; we do this ourselves further down
  ;; Must be set early to prevent ~/.config/emacs/transient from being created
  (let ((data-dir user-emacs-directory))
    (setq transient-levels-file  (concat data-dir "transient/levels")
          transient-values-file  (concat data-dir "transient/values")
          transient-history-file (concat data-dir "transient/history")))
  :config
  (setq transient-default-level 5
        magit-diff-refine-hunk t ; show granular diffs in selected hunk
        ;; Don't autosave repo buffers. This is too magical, and saving can
        ;; trigger a bunch of unwanted side-effects, like save hooks and
        ;; formatters. Trust the user to know what they're doing.
        magit-save-repository-buffers nil
        ;; Don't display parent/related refs in commit buffers; they are rarely
        ;; helpful and only add to runtime costs.
        magit-revision-insert-related-refs nil)
  (add-hook 'magit-process-mode-hook #'goto-address-mode))

(use-package drag-stuff
  :defer 2
  :init
  (general-define-key "<M-up>"    #'drag-stuff-up
                      "<M-down>"  #'drag-stuff-down
                      "<M-left>"  #'drag-stuff-left
                      "<M-right>" #'drag-stuff-right))

(use-package counsel
  :bind
  ("M-x" . counsel-M-x)
  ("C-x C-f" . counsel-find-file)
  ("M-?" . counsel-ag)
  ("C-M-?" . counsel-fzf)
  ("C-c C-c" . counsel-compile)
  ("M-y" . counsel-yank-pop)
  :config
  (add-to-list 'ivy-sort-functions-alist
               '(read-file-name-internal . ivy--sort-by-length))
  ;; Don't use ^ as initial input. Set this here because `counsel' defines more
  ;; of its own, on top of the defaults.
  (setq ivy-initial-inputs-alist nil)
  ;; Integrate with `helpful'
  (setq counsel-describe-function-function #'helpful-callable
        counsel-describe-variable-function #'helpful-variable
        counsel-descbinds-function #'helpful-callable)
  (setq counsel-find-file-ignore-regexp "\\(?:^[#.]\\)\\|\\(?:[#~]$\\)\\|\\(?:^Icon?\\)"))

(use-package dumb-jump
  :commands dumb-jump-result-follow
  :init
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  :config
  (setq dumb-jump-default-project (concat user-emacs-directory "cache/jump")
        dumb-jump-prefer-searcher 'ag
        dumb-jump-aggressive nil
        dumb-jump-selector 'ivy)
  (add-hook 'dumb-jump-after-jump-hook #'better-jumper-set-jump))

(use-package ws-butler
  :config
  (add-hook 'prog-mode-hook #'ws-butler-mode))

(use-package marginalia
  :init
  (marginalia-mode t))

(use-package popwin
  :config
  (popwin-mode t))

(use-package nvm
  :config
  (when (file-exists-p "~/.nvmrc")
    (nvm-use-for)))

(use-package helpful
  :config
  (general-define-key
   "C-h f"   #'helpful-callable
   "C-h v"   #'helpful-variable
   "C-h k"   #'helpful-key
   "C-h x"   #'helpful-command
   "C-c C-d" #'helpful-at-point
   "C-h F"   #'helpful-function)
  (setq counsel-describe-function-function #'helpful-callable
        counsel-describe-variable-function #'helpful-variable))

(use-package iedit
  :config
  (require 'iedit))

(use-package amx
  :init
  (setq amx-save-file (concat user-emacs-directory "cache/amx-items")))

(use-package ace-popup-menu
  :config
  (ace-popup-menu-mode t))

(use-package pulsar
  :defer 2
  :config
  (setq pulsar-pulse t)
  (pulsar-global-mode))

(use-package volatile-highlights
  :config
  (volatile-highlights-mode t))

(use-package buffer-move
  :config
  (setq buffer-move-behavior 'move)
  (general-define-key "<C-S-up>" #'buf-move-up
                      "<C-S-down>" #'buf-move-down
                      "<C-S-left>" #'buf-move-left
                      "<C-S-right>" #'buf-move-right))

(use-package goto-line-preview
  :defer 2
  :config
  (global-set-key [remap goto-line] 'goto-line-preview))

(use-package ido-completing-read+
  :config
  (require 'ido-completing-read+)
  (ido-ubiquitous-mode 1))

(use-package swiper
  :disabled
  :bind
  ("C-s" . swiper))

(use-package dashboard
  :config
  (setq dashboard-set-navigator t
        dashboard-projects-backend 'projectile
        dashboard-show-shortcuts t
        dashboard-items '((recents  . 15)
                          (projects . 10))
        dashboard-center-content t)
  (dashboard-setup-startup-hook))

(use-package savehist
  :config
  (setq history-length 25)
  (savehist-mode))

(use-package vertico
  :custom
  (vertico-resize t)
  (vertico-cycle t)
  :config
  (vertico-mode))

(use-package icomplete
  :custom
  (read-file-name-completion-ignore-case t)
  (read-buffer-completion-ignore-case t)
  (completion-ignore-case t)

  (completion-category-defaults nil)
  (completion-category-overrides
   '((file (styles basic partial-completion))))

  (completion-group t)
  (completions-group-format
        (concat
         (propertize "    " 'face 'completions-group-separator)
         (propertize " %s " 'face 'completions-group-title)
         (propertize " " 'face 'completions-group-separator
                     'display '(space :align-to right)))))

(use-package ivy-xref
  :init
  (setq xref-show-definitions-function #'ivy-xref-show-defs)
  (setq xref-show-xrefs-function #'ivy-xref-show-xrefs))

(use-package emmet-mode
  :after web-mode
  :commands emmet-mode
  :hook
  (web-mode . emmet-mode))

(use-package lsp-mode
  :config
  ;; Annoying stuff
  (setq lsp-enable-links nil)
  (setq lsp-signature-render-documentation nil)
  (setq lsp-headerline-breadcrumb-enable nil)
  (setq lsp-ui-doc-enable nil)
  (setq lsp-lens-enable nil)
  (setq lsp-completion-enable-additional-text-edit nil)
  (setq lsp-enable-symbol-highlighting nil)
  ;; Suggestions from official docs for performance
  (setq lsp-completion-provider :capf)
  (setq lsp-idle-delay 0.500)
  (setq lsp-log-io nil))

(use-package mwim
  :config
  (global-set-key (kbd "C-a") 'mwim-beginning)
  (global-set-key (kbd "C-e") 'mwim-end))

(use-package rainbow-mode
  :hook
  (prog-mode . rainbow-mode))

(use-package bufler
  :defer 1
  :straight (bufler :fetcher github :repo "alphapapa/bufler.el"
                    :files (:defaults (:exclude "helm-bufler.el")))
  :bind (("C-x C-b" . bufler)))

(use-package fancy-compilation
  :defer 1
  :config
  (setq fancy-compilation-override-colors nil)
  (fancy-compilation-mode))

(use-package yafolding
  :defer 2
  :bind
  (("C-`" . yafolding-toggle-element)))

(use-package ace-window
  :config
  (global-set-key (kbd "M-o") 'ace-window))

(use-package dired-sidebar
  :defer 2
  :bind (("<f9>" . dired-sidebar-toggle-sidebar))
  :init
  (add-hook 'dired-sidebar-mode-hook
            (lambda ()
              (unless (file-remote-p default-directory)
                (auto-revert-mode))))
  :config
  (push 'toggle-window-split dired-sidebar-toggle-hidden-commands)
  (push 'rotate-windows dired-sidebar-toggle-hidden-commands)
  (setq dired-sidebar-subtree-line-prefix "__")
  (setq dired-sidebar-theme 'ascii)
  (setq dired-sidebar-use-custom-font t))

(use-package mode-line-bell
  :config
  (mode-line-bell-mode t))

(use-package flycheck
  :bind ("<f7>". flycheck-mode)
  :init (global-flycheck-mode)
  :config
  (setq flycheck-indication-mode 'left-fringe)
  (setq flycheck-highlighting-mode 'sexps)
  (setq flycheck-indication-mode nil))

(use-package sideline
  :hook (flycheck-mode . sideline-mode)
  :init
  (setq sideline-backends-right '(sideline-flycheck)))

(use-package sideline-flycheck
  :hook (flycheck-mode . sideline-flycheck-setup)
  :config
  (setq sideline-flycheck-show-checker-name nil))

(use-package ctrlf
  :config
  (ctrlf-mode +1))

(use-package bm
  :demand t
  :config
  (setq bm-cycle-all-buffers t)
  :bind (("<insert>" . bm-next)
         ("S-<insert>" . bm-previous)
         ("C-<insert>" . bm-toggle)))

(provide 'init)
;;; init.el ends here
