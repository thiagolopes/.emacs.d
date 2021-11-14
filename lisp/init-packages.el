;;; init-packages.el -*- lexical-binding: t; -*-

;;; Code:
(use-package pkgbuild-mode)
(use-package i3wm-config-mode)
(use-package virtualenvwrapper)
(use-package pytest)
(use-package zoom)
(use-package evil)
(use-package web-mode)
(use-package yaml-mode)
(use-package lua-mode)
(use-package resize-window)
(use-package restclient)
(use-package dotenv-mode)


(use-package sudo-edit
  :commands (sudo-edit))


(use-package avy
  :defer t
  :bind ("C-;" . avy-goto-char-timer)
  :custom (avy-timeout-seconds 0.3)
  (avy-style 'pre)
  :custom-face (avy-lead-face ((t (:background "#51afef"
					       :foreground "#870000"
					       :weight bold)))))


(use-package go-mode
  :mode "\\.go\\'"
  :hook (before-save-hook . gofmt-before-save)
  (before-save-hook . lsp-format-buffer)
  (before-save-hook . lsp-organize-imports))


(use-package magit
  :defer t)


(use-package magit-delta
  :after magit
  :hook (magit-mode . magit-delta-mode))


(use-package ivy
  :defer t
  :delight
  :bind ("C-s" . swiper)
  ("M-y" . counsel-yank-pop)
  ("C-x C-f" . counsel-find-file)
  ("C-c C-r" . ivy-resume)
  :custom (ivy-count-format "(%d/%d) ")
  (ivy-use-virtual-buffers t)
  (ivy-height 10)
  (ivy-on-del-error-function nil)
  (ivy-magic-slash-non-match-action 'ivy-magic-slash-non-match-create)
  (ivy-wrap t)
  :config (ivy-mode))


(use-package amx
  :config (amx-mode))


(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))


(use-package flycheck
  :defer t)


(use-package company
  :diminish company-mode
  :init (global-company-mode)
  :custom (company-selection-wrap-around t)
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.1)
  (company-minimum-prefix-length 1)
  (company-tooltip-align-annotations t)
  (company-require-match 'never)
  (company-show-numbers t)
  (company-global-modes '(not shell-mode eaf-mode))
  :config (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous))


(use-package company-box
  :hook (company-mode . company-box-mode))


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


(use-package dap-mode
  :commands (dap-debug dap-debug-edit-template))


(use-package modus-themes
  :custom
  (modus-themes-color 'blue)
  (modus-themes-syntax 'yellow-comments-green-strings)
  (modus-themes-mode-line 'bordeless-3d))


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


(use-package git-gutter
  :defer t
  :config
  (custom-set-variables '(git-gutter:added-sign " ")
			'(git-gutter:deleted-sign " ")
			'(git-gutter:modified-sign " ")
			'(git-gutter:visual-line t))
  :init
  (global-git-gutter-mode +1))


(use-package counsel
  :defer t
  :after ivy
  :bind ("M-?" . counsel-ag)
  ("C-M-?" . counsel-fzf))


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
  :custom (view-read-only t)
  :bind (("<escape>" . view-mode) :map view-mode-map ("n" . forward-line)
	 ("p" . previous-line)
	 ("f" . forward-char)
	 ("b" . backward-char)
	 ("e" . move-end-of-line)
	 ("a" . refined/back-to-indentation-or-beginning)))


(use-package smartparens
  :hook (prog-mode . smartparens-mode))


(use-package dired-subtree
  :config
  (bind-keys :map dired-mode-map
             ("f" . dired-subtree-toggle)))


(use-package multiple-cursors
  :bind (("C->" . mc/mark-next-like-this)
	 ("C-<" . mc/mark-previous-like-this)
	 ("C-c C-c" . mc/edit-lines)))


(use-package minimap
  :config
  (minimap-mode 1)
  (setq minimap-window-location 'left
	minimap-width-fraction 0.0
	minimap-minimum-width 20
	minimap-dedicated-window t
	minimap-enlarge-certain-faces nil))


(use-package telephone-line
  :config
  (telephone-line-mode 1))


(provide 'init-packages)
;;; init-packages ends her
