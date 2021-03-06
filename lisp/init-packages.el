;;; init-packages.el -*- lexical-binding: t; -*-

;;; Code:

(use-package
  pkgbuild-mode)
(use-package
  i3wm-config-mode)
(use-package
  virtualenvwrapper)
(use-package
  blacken)
(use-package
  isortify)
(use-package
  pytest)
(use-package
  zoom)
(use-package
  evil)
(use-package
  web-mode)
(use-package
  yaml-mode)
(use-package
  lua-mode)
(use-package
  resize-window)
(use-package
  restclient)
(use-package
  quelpa-use-package)

(use-package
  minions
  :config (minions-mode 1))

(use-package
  aggressive-indent
  :init (global-aggressive-indent-mode 1))

(use-package
  nyan-mode
  :custom (nyan-animate-nyancat t)
  (nyan-wavy-trail t))

(use-package
  smooth-scrolling
  :init (smooth-scrolling-mode 1))

(use-package
  sudo-edit
  :commands (sudo-edit))

(use-package
  avy
  :defer t
  :bind ("C-;" . avy-goto-char-timer)
  :custom (avy-timeout-seconds 0.3)
  (avy-style 'pre)
  :custom-face (avy-lead-face ((t
				(:background "#51afef"
					     :foreground "#870000"
					     :weight bold))))) ;

(use-package
  go-mode
  :mode "\\.go\\'"
  :hook (before-save-hook . gofmt-before-save)
  (before-save-hook . lsp-format-buffer)
  (before-save-hook . lsp-organize-imports))

(use-package
  beacon
  :init (beacon-mode 1))

(use-package
  magit
  :defer t)

(use-package
  ivy
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

(use-package
  amx
  :config (amx-mode))

(use-package
  rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package
  highlight-numbers
  :config (highlight-numbers-mode))

(use-package
  flycheck
  :defer t
  :init (highlight-numbers-mode 1))

(use-package
  company
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

(use-package
  company-box
  :hook (company-mode . company-box-mode))

(use-package
  projectile
  :delight '(:eval (concat " " (projectile-project-name)))
  :bind ("C-c p" . projectile-command-map)
  :custom (projectile-completion-system 'ivy)
  :config (projectile-mode 1)
  (add-to-list 'projectile-globally-ignored-directories "node_modules"))

(use-package
  which-key
  :init (which-key-mode))

(use-package
  exec-path-from-shell
  :init (setq exec-path-from-shell-arguments nil)
  :config (exec-path-from-shell-initialize))

(use-package
  undo-tree
  :init (global-undo-tree-mode)
  (defalias 'undo! 'undo-tree-visualize))

(use-package
  dap-mode
  :commands (dap-debug dap-debug-edit-template))

(use-package
  modus-themes
  :custom (modus-themes-bold-constructs t)
  (modus-themes-syntax 'yellow-comments-green-strings)
  (modus-themes-mode-line 'bordeless-3d)
  (modus-themes-hl-line 'underline-accented)
  (x-underline-at-descent-line t)
  (modus-themes-paren-match 'subtle-bold))

(use-package
  lsp-mode
  :custom (lsp-prefer-flymake nil)
  (lsp-gopls-staticcheck t)
  (lsp-eldoc-render-all t)
  (lsp-gopls-complete-unimported t)
  (lsp-file-watch-threshold 2000)
  (read-process-output-max (* 1024 1024))
  (lsp-eldoc-hook nil)
  :commands (lsp lsp-deferred)
  :hook ((go-mode python-mode js-mode c-mode web-mode) . lsp-deferred))

(use-package
  lsp-ui
  :after lsp-mode
  :commands lsp-ui-mode
  :custom-face (lsp-ui-doc-background ((t
					(:background nil))))
  (lsp-ui-doc-header ((t
		       (:inherit (font-lock-string-face italic)))))
  :bind (:map lsp-ui-mode-map
	      ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
	      ([remap xref-find-references] . lsp-ui-peek-find-references)
	      ("C-c u" . lsp-ui-imenu))
  :custom (lsp-ui-doc-header t)
  (lsp-ui-peek-enable t)
  (lsp-ui-doc-include-signature t)
  (lsp-ui-doc-border (face-foreground 'default))
  (lsp-ui-flycheck-enable t)
  (lsp-ui-sideline-enable nil)
  (lsp-ui-sideline-ignore-duplicate t)
  (lsp-ui-sideline-show-code-actions nil))

(use-package
  disable-mouse
  :init (global-disable-mouse-mode))

(use-package
  git-gutter+
  :defer t
  :init (global-git-gutter+-mode)
  :config (custom-set-variables '(git-gutter:update-interval 0))
  :commands git-gutter:linum-setup)

(use-package
  counsel
  :defer t
  :after ivy
  :bind ("M-?" . counsel-ag)
  ("C-M-?" . counsel-fzf))

(use-package
  expand-region
  :bind ("M-@" . er/expand-region))

(use-package
  goto-last-change
  :bind ("C-:" . goto-last-change))

(use-package
  clojure-mode
  :ensure t
  :mode (("\\.clj\\'" . clojure-mode)
	 ("\\.edn\\'" . clojure-mode)))

(use-package
  cider
  :after clojure-mode
  :config (add-hook 'clojure-mode-hook #'cider-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'paredit-mode)
  (setq nrepl-log-messages t cider-repl-display-in-current-window t cider-repl-use-clojure-font-lock
	t cider-save-file-on-load t cider-prompt-save-file-on-load 'always-save
	cider-font-lock-dynamically '(macro core function var) nrepl-hide-special-buffers t
	cider-overlays-use-font-lock t)
  (cider-repl-toggle-pretty-printing))

(use-package
  fira-code-mode
  :custom (fira-code-mode-disabled-ligatures '("[]" "#_" "#_(" "x" "#("))
  :config (global-fira-code-mode))

(use-package
  centaur-tabs
  :config (centaur-tabs-headline-match)
  (setq centaur-tabs-build-helm-source t)
  (setq centaur-tabs-height 28 x-underline-at-descent-line t centaur-tabs-plain-icons t
	centaur-tabs-set-modified-marker t centaur-tabs-set-icons t centaur-tabs-style "alternate"
	uniquify-separator "/" uniquify-buffer-name-style 'forward centaur-tabs-cycle-scope 'tabs)
  :bind ("C-M-n" . centaur-tabs-backward)
  ("C-M-p" . centaur-tabs-forward)
  :init (centaur-tabs-mode t))

(use-package
  scala-mode
  :interpreter ("scala" . scala-mode))

(use-package
  winner
  :ensure nil
  :custom (winner-boring-buffers '("*Completions*" "*Compile-Log*" "*inferior-lisp*"
				   "*Fuzzy Completions*" "*Apropos*" "*Help*" "*cvs*"
				   "*Buffer List*" "*Ibuffer*" "*esh command on file*"))
  :config (winner-mode 1))

(use-package
  page-break-lines
  :diminish
  :init (global-page-break-lines-mode))

(use-package
  quickrun
  :bind (("<f5>" . quickrun)
	 ("M-<f5>" . quickrun-shell)))

(use-package
  view
  :custom (view-read-only t)
  :bind (("<escape>" . view-mode) :map view-mode-map ("n" . forward-line)
	 ("p" . previous-line)
	 ("f" . forward-char)
	 ("b" . backward-char)
	 ("e" . move-end-of-line)
	 ("a" . refined/back-to-indentation-or-beginning)))

(use-package
  smartparens
  :hook (prog-mode . smartparens-mode))

(use-package dired+
  :quelpa (dired+ :fetcher github :repo "emacsmirror/dired-plus")
  :ensure nil
  :init
  (setq diredp-hide-details-initially-flag nil)
  (setq diredp-hide-details-propagate-flag nil)
  :config
  (diredp-toggle-find-file-reuse-dir 1))

(provide 'init-packages)
;;; init-packages ends her
