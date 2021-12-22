;;; init-packages.el -*- lexical-binding: t; -*-

;;; Code:
(use-package pkgbuild-mode)
(use-package i3wm-config-mode)
(use-package virtualenvwrapper)
(use-package pytest)
(use-package evil)
(use-package web-mode)
(use-package yaml-mode)
(use-package lua-mode)
(use-package restclient)
(use-package dotenv-mode)
(use-package realgud)
(use-package git-timemachine)
(use-package browse-at-remote)
(use-package pdf-tools)
(use-package docker)
(use-package python-black)
(use-package isortify)
(use-package rmsbolt)

(use-package centered-cursor-mode
  :init
  (global-centered-cursor-mode))

(use-package highlight-parentheses
  :hook
  (prog-mode-hook . highlight-parentheses-mode))

(use-package solaire-mode
  :init
  (solaire-global-mode +1))

(use-package ace-window
  :bind
  ("M-o" . ace-window))

(use-package lsp-ui
  :after lsp
  :init
  (lsp-ui-mode)
  :custom
  (lsp-ui-doc-enable t)
  (lsp-ui-doc-use-webkit t))

(use-package realgud-ipdb
  :after realgud)

(use-package sudo-edit
  :commands (sudo-edit))

(use-package avy
  :bind
  ("C-;" . avy-goto-word-0)
  :custom
  (avy-background t)
  (avy-timeout-seconds 0.3))

(use-package go-mode
  :mode "\\.go\\'"
  :hook (before-save-hook . gofmt-before-save)
  (before-save-hook . lsp-format-buffer)
  (before-save-hook . lsp-organize-imports))

(use-package magit
  :config
  (defalias 'git 'magit)
  :defer t)

(use-package magit-delta
  :after magit
  :hook (magit-mode . magit-delta-mode))

(use-package flycheck
  :defer t)

(use-package projectile
  :delight '(:eval (concat " " (projectile-project-name)))
  :bind ("C-c p" . projectile-command-map)
  :custom (projectile-completion-system 'ivy)
  :config (projectile-mode 1)
  (add-to-list 'projectile-globally-ignored-directories "node_modules"))

(use-package exec-path-from-shell
  :init (setq exec-path-from-shell-arguments nil)
  :config (exec-path-from-shell-initialize))

(use-package undo-tree
  :init (global-undo-tree-mode)
  (defalias 'undo! 'undo-tree-visualize))

(use-package lsp-mode
  :custom (lsp-prefer-flymake nil)
  (lsp-gopls-staticcheck t)
  (lsp-eldoc-render-all t)
  (lsp-gopls-complete-unimported t)
  (lsp-file-watch-threshold 2000)
  (read-process-output-max (* 1024 1024))
  (lsp-eldoc-hook nil)
  :commands (lsp lsp-deferred)
  :hook ((go-mode python-mode js-mode c-mode web-mode) . lsp-deferred))

(use-package diff-hl
  :custom
  (diff-hl-draw-borders nil)

  :init
  (global-diff-hl-mode))

(use-package vertico
  :custom
  (vertico-cycle 1)
  :init
  (vertico-mode))

(use-package marginalia
  :after vertico
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :init
  (marginalia-mode))

(use-package expand-region
  :bind ("M-@" . er/expand-region))

(use-package goto-last-change
  :bind ("C-:" . goto-last-change))

(use-package clojure-mode
  :ensure t
  :mode (("\\.clj\\'" . clojure-mode)
	 ("\\.edn\\'" . clojure-mode)))

(use-package cider
  :after clojure-mode
  :config (add-hook 'clojure-mode-hook #'cider-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'paredit-mode)
  (setq nrepl-log-messages t cider-repl-display-in-current-window t cider-repl-use-clojure-font-lock
	t cider-save-file-on-load t cider-prompt-save-file-on-load 'always-save
	cider-font-lock-dynamically '(macro core function var) nrepl-hide-special-buffers t
	cider-overlays-use-font-lock t)
  (cider-repl-toggle-pretty-printing))

(use-package view
  :custom
  (view-read-only t)
  :bind
  (("<escape>" . view-mode)
   :map view-mode-map
   ("n" . forward-line)
   ("p" . previous-line)
   ("f" . forward-char)
   ("b" . backward-char)
   ("a" . mwim-beginning)
   ("e" . mwim-end)
   ("j" . backward-char)
   ("k" . forward-line)
   ("l" . previous-line)
   ("ç" . forward-char)))

(use-package multiple-cursors
  :bind (("C->" . mc/mark-next-like-this)
	 ("C-<" . mc/mark-previous-like-this)
	 ("C-c C-c" . mc/edit-lines)))

(use-package doom-modeline
  :config
  (doom-modeline-mode 1))

(use-package corfu
  :after orderless
  :custom
  (corfu-quit-at-boundary nil)
  (corfu-quit-no-match t)
  (corfu-cycle t)
  (corfu-auto t)
  :init
  (corfu-global-mode))

(use-package orderless
  :init
  (setq completion-styles '(orderless)
	completion-category-defaults nil
	completion-category-overrides '((file (styles . (partial-completion)))))
  :config
  (setq completion-styles '(orderless)
	completion-category-overrides '((file (styles basic partial-completion)))))

(use-package savehist
  :init
  (savehist-mode))

(use-package eyebrowse
  :init (eyebrowse-mode))

(use-package helpful
  :bind
  ("C-h f" . helpful-function)
  ("C-h v" . helpful-variable)
  ("C-h k" . helpful-key))

(use-package ztree
  :bind
  ("C-x d". ztree-dir)
  ("C-x C-d". ztree-dir))

(use-package counsel
  :commands (counsel-yank-pop counsel-ag counsel-fzf)
  :bind
  ("M-y" . counsel-yank-pop)
  ("M-?" . counsel-ag)
  ("C-M-?" . counsel-fzf))

(use-package ctrlf
  :init
  (ctrlf-mode))

(use-package mwim
  :bind
  ("C-a" . mwim-beginning)
  ("C-e" . mwim-end))

(use-package highlight-thing
  :hook
  (prog-mode-hook . highlight-thing-mode))

(use-package goto-line-preview
  :bind
  ([remap  goto-line] . goto-line-preview))

(use-package rainbow-delimiters
  :hook
  (prog-mode-hook . rainbow-delimiters-mode))

(use-package hungry-delete
  :hook
  (prog-mode-hook . hungry-delete-mode))

(use-package zzz-to-char
  :bind
  ("M-z" . zzz-to-char))

(use-package indent-guide
  :custom
  (indent-guide-delay 0.2))

(use-package whitespace-cleanup-mode
  :init
  (global-whitespace-cleanup-mode))

(use-package guess-language
  :init
  (guess-language-mode)
  :custom
  (guess-language-languages '(en pt))
  (guess-language-min-paragraph-length 35))

(use-package git-messenger
  :config
  (defalias 'git-message 'git-messenger:popup-message))

(use-package zenburn-theme)

(use-package doom-themes
  :config
  (load-theme 'doom-monokai-classic))

(use-package all-the-icons
  :if (display-graphic-p))

(use-package yascroll
  :init
  (global-yascroll-bar-mode 1)
  :custom
  (yascroll:delay-to-hide nil))

(use-package centaur-tabs
  :hook
  (prog-mode-hook . centaur-tabs-local-mode)
  :custom
  (centaur-tabs-plain-icons t)
  (centaur-tabs-show-new-tab-button nil)
  (centaur-tabs-set-bar 'over)
  (centaur-tabs-set-close-button nil)
  (centaur-tabs-set-icons t)
  (centaur-tabs-set-modified-marker t)
  (centaur-tabs-modified-marker "*")
  (centaur-tags-style "zigzag")
  (centaur-tabs-icon-scale-factor 0.7)
  (centaur-tabs-cycle-scope 'tabs)
  :config
  (centaur-tabs-headline-match)
  :init
  (centaur-tabs-mode t)
  :bind
  ("s-<tab>" . centaur-tabs-forward)
  ("<s-iso-lefttab>" . centaur-tabs-backward))

(provide 'init-packages)
;;; init-packages ends her
