;;; init-packages.el -*- lexical-binding: t; -*-

;;; Code:

(use-package pkgbuild-mode)
(use-package clojure-mode)
(use-package sudo-edit)
(use-package virtualenvwrapper)
(use-package zoom)

(use-package magit
  :defer t)

(use-package ivy
  :defer t
  :delight
  :custom
  (ivy-count-format "(%d/%d) ")
  (ivy-use-virtual-buffers t)
  :config
  (ivy-mode))

(use-package highlight-parentheses
  :init
  (global-highlight-parentheses-mode))

(use-package rainbow-delimiters
  :init
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package highlight-numbers
  :init
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

(use-package projectile
  :bind
  ("C-c C-p" . projectile-command-map)
  :config
  (projectile-mode 1))

(use-package which-key
  :config
  (which-key-mode))

(use-package exec-path-from-shell
  :init (setq exec-path-from-shell-arguments nil)
  :config (exec-path-from-shell-initialize))

(use-package undo-tree
  :init (global-undo-tree-mode)
  (defalias 'undo! 'undo-tree-visualize))

(use-package helm
  :init
  :custom
  (helm-buffer-max-length 5)
  (helm-candidate-number-limit 5)
  :config
  (helm-autoresize-mode 1)
  :bind
  ("M-x" . helm-M-x)
  ("M-y" . helm-show-kill-ring)
  ("C-x b" . helm-mini)
  ("C-x C-f" . helm-find-files)
  (:map helm-map
        ("C-o" . nil)
        ("TAB" . helm-execute-persistent-action)
        ("C-i" . helm-execute-persistent-action)
        ("C-z" . helm-select-action)
        ("C-h" . delete-backward-char)))

(use-package helm-swoop
  :after helm
  :custom
  (helm-swoop-speed-or-color t)
  :bind
  ("C-s" . helm-swoop))

(use-package dap-mode
  :diminish
  :commands (dap-debug dap-debug-edit-template))

(use-package cyberpunk-theme
  :init
  (load-theme 'cyberpunk))

(use-package lsp-mode
  :custom
  (lsp-prefer-flymake nil)
  :hook
  (prog-mode . lsp-mode))

(use-package lsp-ui
  :diminish
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
  :init
  (global-git-gutter+-mode)
  :config
  (custom-set-variables
   '(git-gutter:update-interval 0))
  :commands
  git-gutter:linum-setup)

(use-package counsel
  :bind
  ("M-?" . counsel-ag)
  ("C-M-?" . counsel-fzf))

(use-package expand-region
  :bind
  ("M-@" . er/expand-region))

(use-package goto-last-change
  :bind
  ("C-:" . goto-last-change))

(provide 'init-packages)
;;; init-packages ends here
