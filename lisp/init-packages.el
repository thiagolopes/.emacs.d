;;; init-packages.el -*- lexical-binding: t; -*-

;;; Code:
(use-package pkgbuild-mode)
(use-package i3wm-config-mode)
(use-package pytest)
(use-package evil)
(use-package web-mode)
(use-package graphql-mode)
(use-package yaml-mode)
(use-package lua-mode)
(use-package typescript-mode)
(use-package restclient)
(use-package dotenv-mode)
(use-package realgud)
(use-package git-timemachine)
(use-package browse-at-remote)
(use-package pdf-tools)
(use-package python-black)
(use-package isortify)
(use-package rmsbolt)
(use-package mermaid-mode)
(use-package org-modern)
(use-package diminish)
(use-package eldoc)
(use-package git-undo)

(use-package pyenv
  :config
  (defalias 'workon 'pyvenv-workon))

(use-package nyan-mode
  :disabled
  :config
  (nyan-mode))

(use-package ace-window
  :bind
  ("M-o" . ace-window))

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
  :custom
  (magit-diff-paint-whitespace nil))

(use-package flycheck
  :config
  (setq flycheck-flake8rc ".flake8")
  :init
  (global-flycheck-mode))

(use-package projectile
  :diminish
  :init
  (projectile-mode t)
  :bind
  ("<f3>" . projectile-dired)
  :custom
  (projectile-completion-system 'ivy)
  (add-to-list 'projectile-globally-ignored-directories "node_modules"))

(use-package exec-path-from-shell
  :init (setq exec-path-from-shell-arguments nil)
  :config (exec-path-from-shell-initialize))

(use-package undo-tree
  :diminish
  :init
  (global-undo-tree-mode t)
  (defalias 'undo! 'undo-tree-visualize)
  :custom
  (undo-tree-visualizer-diff t)
  (undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))
  (undo-tree-visualizer-timestamps t))

(use-package diff-hl
  :custom
  (diff-hl-draw-borders nil)
  :init
  (global-diff-hl-mode t))

(use-package vertico
  :custom
  (vertico-cycle 1)
  :init
  (vertico-mode t))

(use-package marginalia
  :after vertico
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :init
  (marginalia-mode t))

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
  :diminish "Vim-Like-Modal"
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

(use-package company
  :config
  (global-company-mode))

(use-package company-box
  :hook (company-mode . company-box-mode))

(use-package company-statistics
  :hook
  (company-mode . company-statistics-mode))

(use-package orderless
  :init
  (setq completion-styles '(orderless)
	completion-category-defaults nil
	completion-category-overrides '((file (styles . (partial-completion))))))

(use-package savehist
  :custom
  (undo-tree-auto-save-history t)
  (undo-tree-enable-undo-in-region nil)
  :init
  (savehist-mode 1))

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
  :diminish
  :init
  (global-whitespace-cleanup-mode))

(use-package guess-language
  :diminish
  :init
  (guess-language-mode)
  :custom
  (guess-language-languages '(en pt))
  (guess-language-min-paragraph-length 8))

(use-package git-messenger
  :config
  (defalias 'git-logs 'git-messenger:popup-message))

(use-package doom-themes
  :disabled
  :config
  (load-theme 'doom-monokai-classic))

(use-package monokai-theme
  :disabled
  :config
  (load-theme 'monokai t))

(use-package doom-modeline
  :disabled
  :init
  (doom-modeline-mode 1))

(use-package all-the-icons
  :if (display-graphic-p))

(use-package highlight-indent-guides
  :diminish
  :config
  (defun my-highlighter (level responsive display)
  (if (> 1 level)
      nil
    (highlight-indent-guides--highlighter-default level responsive display)))
  (setq highlight-indent-guides-highlighter-function 'my-highlighter)
  :init
  (add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
  (setq highlight-indent-guides-method 'character))

(use-package markdown-mode
  :ensure t
  :mode (("README\\.md\\'" . gfm-mode)
	 ("\\.md\\'" . markdown-mode)
	 ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(use-package hl-todo
  :custom-face
  (hl-todo ((t (:inherit hl-todo :italic t))))
  :hook ((prog-mode . hl-todo-mode)
	 (yaml-mode . hl-todo-mode)))

(use-package minions
  :init
  (minions-mode))

(use-package pulsar
  :init
  (pulsar-global-mode))

(use-package keycast
  :init
  (defun +toggle-keycast()
    (interactive)
    (if (member '("" keycast-mode-line " ") global-mode-string)
	(progn (setq global-mode-string (delete '("" keycast-mode-line " ") global-mode-string))
	       (remove-hook 'pre-command-hook 'keycast--update))
      (add-to-list 'global-mode-string '("" keycast-mode-line " "))
      (add-hook 'pre-command-hook 'keycast--update t)))
  :config
  (+toggle-keycast))

(use-package modus-themes
  :init
  (modus-themes-load-themes)
  :config
  (modus-themes-load-vivendi)
  :custom
  (modus-themes-italic-constructs t)
  (modus-themes-bold-constructs nil)
  (modus-themes-markup '(background italic))
  (modus-themes-region '(bg-only no-extend))
  (modus-themes-mode-line '(accented 3d borderless (padding . 5) (height . 0.9)))
  (modus-themes-syntax '(yellow-comments))
  (modus-themes-paren-match '(bold intense underline))
  :bind
  ("<f5>" . modus-themes-toggle))

(use-package super-save
  :diminish
  :config
  (super-save-mode +1))

(use-package clean-kill-ring
  :straight (clean-kill-ring :type git :host github :repo "NicholasBHubbard/clean-kill-ring.el")
  :custom
  (clean-kill-ring-prevent-duplicates 1)
  :config
  (clean-kill-ring-mode 1))

(use-package smart-mode-line
  :config
  (smart-mode-line-enable 1))

(use-package mini-modeline
  :diminish
  :after smart-mode-line
  :config
  (mini-modeline-mode t))

(use-package shell-pop
  :custom
   (shell-pop-shell-type '("ansi-term" "*ansi-term*" (lambda nil (ansi-term shell-pop-term-shell))))
  :bind
  ("<f2>" . shell-pop))

(use-package eglot)

(use-package eldoc-box
  :after eldoc eglot
  :config
  (add-hook 'eglot-managed-mode-hook #'eldoc-box-hover-mode t)
  :init
  (eldoc-box-hover-at-point-mode t))

(use-package flycheck-popup-tip
  :after flycheck
  :config
  (add-hook 'flycheck-mode-hook 'flycheck-popup-tip-mode))

(use-package elpy
  :config
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (setq python-shell-interpreter "ipython")
  (setq python-shell-interpreter-args "-i --simple-prompt")
  (setq python-shell-prompt-detect-failure-warning nil)
  (add-hook 'elpy-mode-hook 'flycheck-mode)
  (add-to-list 'python-shell-completion-native-disabled-interpreters "jupyter")
  (advice-add 'python-mode :before 'elpy-enable))

(use-package counsel
  :commands (counsel-yank-pop counsel-ag counsel-fzf)
  :bind
  ("M-y" . counsel-yank-pop)
  ("M-?" . counsel-ag)
  ("C-M-?" . counsel-fzf))

(provide 'init-packages)
;;; init-packages.el ends here
