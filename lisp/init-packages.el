;;; init-packages.el -*- lexical-binding: t; -*-

;;; Code:

(use-package pkgbuild-mode)
(use-package i3wm-config-mode)
(use-package sudo-edit)
(use-package virtualenvwrapper)
(use-package zoom)

(use-package magit
  :defer t)

(use-package ivy
  :defer t
  :delight
  :bind
  ("C-s" . swiper)
  ("M-y" . counsel-yank-pop)
  ("C-x C-f" . counsel-find-file)
  ("C-c C-r" . ivy-resume)
  :custom
  (ivy-count-format "(%d/%d) ")
  (ivy-use-virtual-buffers t)
  :config
  (ivy-mode))

(use-package smex
  :config
  (smex-initialize)
  :bind
  ("M-x" . smex))

(use-package highlight-parentheses
  :init
  (global-highlight-parentheses-mode))

(use-package rainbow-delimiters
  :init
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package highlight-numbers
  :config
  (highlight-numbers-mode))

(use-package flycheck
  :defer t
  :init
  (highlight-numbers-mode 1))

(use-package company
  :init
  (global-company-mode)
  :config
  (setq company-selection-wrap-around t)
  (setq company-minimum-prefix-length 1)
  (setq company-idle-delay 0)
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous))

(use-package company-box
  :hook
  (company-mode . company-box-mode))

(use-package projectile
  :delight '(:eval (concat " " (projectile-project-name)))
  :bind
  ("C-c p" . projectile-command-map)
  :config
  (projectile-mode 1)
  (setq projectile-completion-system 'ivy))

(use-package which-key
  :init
  (which-key-mode))

(use-package exec-path-from-shell
  :init (setq exec-path-from-shell-arguments nil)
  :config (exec-path-from-shell-initialize))

(use-package undo-tree
  :init (global-undo-tree-mode)
  (defalias 'undo! 'undo-tree-visualize))

(use-package dap-mode
  :commands (dap-debug dap-debug-edit-template))

(use-package solarized-theme
  :disabled ;; disable until centaur support
  :init
  (load-theme 'solarized-dark)
  :custom
  (solarized-use-variable-pitch nil)
  (solarized-use-more-italic t)
  (solarized-scale-org-headlines nil)
  (solarized-emphasize-indicators nil)
  (x-underline-at-descent-line t))

(use-package chocolate-theme
  :disabled
  :ensure t
  :config
  (load-theme 'chocolate t))

(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-wilmersdorf t))

(use-package lsp-mode
  :custom
  (lsp-prefer-flymake nil)
  :hook
  (prog-mode . lsp-mode))

(use-package lsp-ui
  :after
  lsp-mode
  :commands
  lsp-ui-mode
  :custom-face
  (lsp-ui-doc-background ((t (:background nil))))
  (lsp-ui-doc-header ((t (:inherit (font-lock-string-face italic)))))
  :bind
  (:map lsp-ui-mode-map
	([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
	([remap xref-find-references] . lsp-ui-peek-find-references)
	("C-c u" . lsp-ui-imenu))
  :custom
  (lsp-ui-doc-enable t)
  (lsp-ui-doc-header t)
  (lsp-ui-doc-include-signature t)
  (lsp-ui-doc-position 'top)
  (lsp-ui-sideline-enable nil)
  (lsp-ui-sideline-ignore-duplicate t)
  (lsp-ui-sideline-show-code-actions nil)
  :config
  (setq lsp-ui-sideline-ignore-duplicate t))

(use-package disable-mouse
  :init
  (global-disable-mouse-mode))

(use-package git-gutter+
  :defer t
  :init
  (global-git-gutter+-mode)
  :config
  (custom-set-variables
   '(git-gutter:update-interval 0))
  :commands
  git-gutter:linum-setup)

(use-package counsel
  :defer t
  :after ivy
  :bind
  ("M-?" . counsel-ag)
  ("C-M-?" . counsel-fzf))

(use-package expand-region
  :bind
  ("M-@" . er/expand-region))

(use-package goto-last-change
  :bind
  ("C-:" . goto-last-change))

(use-package smart-mode-line
   :custom
   (sml/theme 'respectful)
   :config
   (sml/setup))

(use-package clojure-mode
  :ensure t
  :mode (("\\.clj\\'" . clojure-mode)
         ("\\.edn\\'" . clojure-mode)))

(use-package cider
  :after clojure-mode
  :config
  (add-hook 'clojure-mode-hook #'cider-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'paredit-mode)
  (setq nrepl-log-messages t
        cider-repl-display-in-current-window t
        cider-repl-use-clojure-font-lock t
	cider-save-file-on-load t
        cider-prompt-save-file-on-load 'always-save
        cider-font-lock-dynamically '(macro core function var)
        nrepl-hide-special-buffers t
        cider-overlays-use-font-lock t)
  (cider-repl-toggle-pretty-printing))

(use-package fira-code-mode
  :custom (fira-code-mode-disabled-ligatures '("[]" "#_" "#_(" "x"))
  :config (global-fira-code-mode))

(use-package centaur-tabs
  :config
  (centaur-tabs-headline-match)
  (setq centaur-tabs-build-helm-source t)
  (setq centaur-tabs-height 28
	x-underline-at-descent-line t
	centaur-tabs-plain-icons t
	centaur-tabs-set-modified-marker t
	centaur-tabs-set-icons t
	centaur-tabs-style "alternate"
	uniquify-separator "/"
	uniquify-buffer-name-style 'forward
	centaur-tabs-cycle-scope 'tabs)
  :bind
  ("C-M-n" . centaur-tabs-backward)
  ("C-M-p" . centaur-tabs-forward)
  :init
  (centaur-tabs-mode t))

(provide 'init-packages)
;;; init-packages ends here
