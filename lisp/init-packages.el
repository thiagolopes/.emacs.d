;;; init-packages.el -*- lexical-binding: t; -*-

;;; Code:
(use-package diminish)
(use-package pkgbuild-mode)
(use-package i3wm-config-mode)
(use-package web-mode)
(use-package graphql-mode)
(use-package yaml-mode)
(use-package lua-mode)
(use-package typescript-mode)
(use-package dotenv-mode)
(use-package mermaid-mode)
(use-package dockerfile-mode)
(use-package json-mode)
(use-package restclient)
(use-package git-timemachine)
(use-package browse-at-remote)
(use-package pdf-tools)
(use-package python-black)
(use-package isortify)
(use-package pyenv)

(use-package highlight-numbers
  :hook (prog-mode . highlight-numbers-mode))

(use-package highlight-escape-sequences
  :hook (prog-mode . hes-mode))

(use-package delsel
  :config (delete-selection-mode t))

(use-package recentf
  :custom
  (recentf-max-saved-items 50)
  :init
  (recentf-mode))

(use-package bookmark
  :defer t
  :custom
  (bookmark-fontify nil))

(use-package ace-window
  :bind
  ("M-o" . ace-window))

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
  :hook (before-save-hook . gofmt-before-save))

(use-package magit)

(use-package flycheck
  :custom
  (flycheck-flake8rc ".flake8")
  :config (global-flycheck-mode +1))

(use-package projectile
  :diminish
  :init
  (projectile-mode t)
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
  :disabled
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
  :custom
  (company-idle-delay 0.1)
  (company-selection-wrap-around t)
  (company-tooltip-align-annotations t)
  (company-frontends '(company-pseudo-tooltip-frontend
                       company-echo-metadata-frontend))
  :config
  (define-key company-active-map (kbd "C-n") #'company-select-next)
  (define-key company-active-map (kbd "C-p") #'company-select-previous)
  (define-key company-active-map (kbd "M-/") #'company-complete)
  (define-key company-active-map (kbd "M-.") #'company-show-location)
  (define-key company-active-map (kbd "RET") nil)
  (global-company-mode))

(use-package company-box
  :diminish
  :hook (company-mode . company-box-mode))

(use-package orderless
  :init
  (setq completion-styles '(orderless)
	completion-category-defaults nil
	completion-category-overrides '((file (styles . (partial-completion))))))

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
  :disabled
  :bind
  ("C-a" . mwim-beginning)
  ("C-e" . mwim-end))

(use-package highlight-thing
  :init
  (global-highlight-thing-mode t))

(use-package goto-line-preview
  :bind
  ([remap  goto-line] . goto-line-preview))

(use-package hungry-delete
  :disabled
  :hook
  (prog-mode-hook . hungry-delete-mode))

(use-package zzz-to-char
  :bind
  ("M-z" . zzz-to-char))

(use-package whitespace-cleanup-mode
  :diminish
  :init
  (global-whitespace-cleanup-mode))

(use-package git-messenger
  :config
  (defalias 'git-logs 'git-messenger:popup-message))

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
  :disabled
  :init
  (modus-themes-load-themes)
  :config
  (modus-themes-load-vivendi)
  :custom
  (modus-themes-italic-constructs t)
  (modus-themes-bold-constructs nil)
  (modus-themes-markup '(background italic))
  (modus-themes-region '(bg-only no-extend))
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

(use-package shell-pop
  :custom
   (shell-pop-shell-type '("ansi-term" "*ansi-term*" (lambda nil (ansi-term shell-pop-term-shell))))
  :bind
  ("<f2>" . shell-pop))

(use-package eglot)

(use-package elpy
  :config
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (setq python-shell-interpreter "ipython")
  (setq python-shell-interpreter-args "-i --simple-prompt")
  (setq python-shell-prompt-detect-failure-warning nil)
  (add-hook 'elpy-mode-hook 'flycheck-mode)
  (add-hook 'elpy-mode-hook (lambda () (highlight-indentation-mode -1)))
  (add-to-list 'python-shell-completion-native-disabled-interpreters "jupyter")
  (advice-add 'python-mode :before 'elpy-enable))

(use-package counsel
  :commands (counsel-yank-pop counsel-ag counsel-fzf)
  :bind
  ("M-?" . counsel-ag)
  ("C-M-?" . counsel-fzf))

(use-package helm
  :custom
  (helm-autoresize-mode nil)
  (helm-M-x-fuzzy-match t)
  (helm-buffers-fuzzy-matching t)
  (helm-recentf-fuzzy-match t)
  (history-delete-duplicates t)
  :bind
  (("M-x" . helm-M-x)
   ("M-y" . helm-show-kill-ring)
   ("C-x C-f" . helm-find-files)
   :map helm-map
   ("<tab>" . helm-execute-persistent-action)
   ("C-i" . helm-execute-persistent-action)
   ("C-z" . helm-select-action)))

(use-package zenburn-theme
  :init
  (load-theme 'zenburn))

(use-package key-chord
  :init
  (key-chord-mode t)
  :config
  (key-chord-define-global "1q" "!")
  (key-chord-define-global "2w" "@")
  (key-chord-define-global "3e" "#")
  (key-chord-define-global "4r" "$")
  (key-chord-define-global "5t" "%")
  (key-chord-define-global "6y" "^")
  (key-chord-define-global "6t" "^")
  (key-chord-define-global "7y" "&")
  (key-chord-define-global "8u" "*")
  (key-chord-define-global "9i" "(")
  (key-chord-define-global "0o" ")")
  (key-chord-define-global "-p" "_"))

(use-package smart-mode-line
  :config
  (sml/setup)
  (sml/apply-theme 'automatic))

(use-package aggressive-indent
  :hook
  (emacs-lisp-mode-hook . aggressive-indent-mode))

(use-package which-key
  :config
  (setq which-key-sort-order 'which-key-key-order-alpha)
  (which-key-setup-side-window-right)
  :init
  (which-key-mode))

(use-package paredit
  :hook
  (emacs-lisp-mode-hook . enable-paredit-mode)
  (eval-expression-minibuffer-setup-hook . enable-paredit-mode)
  (ielm-mode-hook . enable-paredit-mode)
  (lisp-mode-hook . enable-paredit-mode)
  (lisp-interaction-mode-hook . enable-paredit-mode)
  (scheme-mode-hook . enable-paredit-mode))

(use-package consult
  :bind
  (("C-x b" . consult-buffer)
   ("C-s" . consult-line)
   :map minibuffer-local-map
   ("C-r" . consult-history))
  :custom
  (completion-in-region-function #'consult-completion-in-region))

(provide 'init-packages)
;;; init-packages.el ends here
