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
(use-package virtualenvwrapper)
(use-package eldoc)
(use-package git-undo)

(use-package aweshell
  :straight (aweshell :type git :host github :repo "manateelazycat/aweshell")
  :bind
  ("<f2>" . aweshell-dedicated-toggle))

(use-package nyan-mode
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
  :disabled
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
  :init
  (global-flycheck-mode))

(use-package projectile
  :diminish
  :init (projectile-mode 1)
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
  (global-undo-tree-mode)
  (defalias 'undo! 'undo-tree-visualize)
  :custom
  (undo-tree-visualizer-diff t)
  (undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))
  (undo-tree-visualizer-timestamps t))

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

(use-package corfu
  :custom
  (tab-always-indent 'complete)
  (completion-cycle-threshold nil)
  (corfu-auto nil)
  (corfu-min-width 30)
  (corfu-max-width corfu-min-width)
  (corfu-count 14)
  (corfu-scroll-margin 4)
  (corfu-cycle t)
  (corfu-quit-at-boundary nil)
  (corfu-quit-no-match 'separator)
  (corfu-separator ?\s)
  (corfu-preview-current 'insert)
  (corfu-preselect-first t)
  :init
  (global-corfu-mode)
  :config
  (defun corfu-enable-always-in-minibuffer ()
    "Enable Corfu in the minibuffer if Vertico/Mct are not active."
    (unless (or (bound-and-true-p mct--active)
		(bound-and-true-p vertico--input))
      (setq-local corfu-auto nil)
      (corfu-mode 1)))
  (add-hook 'minibuffer-setup-hook #'corfu-enable-always-in-minibuffer 1))

(use-package kind-icon
  :after corfu
  :custom
  (kind-icon-use-icons t)
  (kind-icon-default-face 'corfu-default)
  (kind-icon-blend-background nil)
  (kind-icon-blend-frac 0.08)
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package corfu-doc
  :after corfu
  :hook (corfu-mode . corfu-doc-mode)
  :bind
  (:map corfu-map
	([remap corfu-show-documentation] . corfu-doc-toggle)
	("M-n" . corfu-doc-scroll-up)
	("M-p" . corfu-doc-scroll-down))
  :custom
  (corfu-doc-delay 0.2)
  (corfu-doc-max-width 80)
  (corfu-doc-max-height 20)
  (corfu-echo-documentation nil))

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
  ("C-;" . zzz-to-char))

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

(use-package eglot
  :custom
  (completion-category-overrides '((eglot (styles orderless)))))

(use-package keycast
  :init
  (defun +toggle-keycast()
    (interactive)
    (if (member '("" keycast-mode-line " ") global-mode-string)
	(progn (setq global-mode-string (delete '("" keycast-mode-line " ") global-mode-string))
	       (remove-hook 'pre-command-hook 'keycast--update)
	       (message "Keycast OFF"))
      (add-to-list 'global-mode-string '("" keycast-mode-line " "))
      (add-hook 'pre-command-hook 'keycast--update t)
      (message "Keycast ON")))
  :config
  (+toggle-keycast))

(use-package counsel
  :commands (counsel-yank-pop counsel-ag counsel-fzf)
  :bind
  ("M-y" . counsel-yank-pop)
  ("M-?" . counsel-ag)
  ("C-M-?" . counsel-fzf))

(use-package modus-themes
  :init
  (setq modus-themes-italic-constructs t
	modus-themes-mixed-fonts t
	modus-themes-bold-constructs t
	modus-themes-region '(bg-only no-extend))
  (modus-themes-load-themes)
  :config
  (modus-themes-load-vivendi)
  :bind ("<f5>" . modus-themes-toggle))

(use-package super-save
  :config
  (super-save-mode +1))

(provide 'init-packages)
;;; init-packages.el ends here
