;;; init-packages.el -- init file only with use-package  -*- lexical-binding: t; -*-
;;; Commentaryp:
;;;  this package will run after early-init.el
;;; Code:
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(eval-and-compile
  (require 'use-package)
  (setq use-package-always-ensure t
	use-package-expand-minimally t))


;; package periodic update
(use-package auto-package-update
  :custom
  (auto-package-update-interval 6)
  (auto-package-update-delete-old-versions t)
  :config
  (package-refresh-contents :async)
  (auto-package-update-maybe))

;; navegation between line/end line
(use-package mwim
  :bind (("C-a" . mwim-beginning)
	 ("C-e" . mwim-end)))

;; debug startup
(use-package esup :defer t)

;; better defaults
(use-package crux
  :bind
  (("C-k" . crux-smart-kill-line)))

;; load .env shell
(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

;; font/theme
(use-package fontaine
  :if (display-graphic-p)
  :config
  (setq fontaine-presets
	'((hack
	   :default-family "Hack"
	   :bold-weight extrabold)
	  (iosevka
	   :default-family "Iosevka")
	  (iosevka-confy
	   :default-family "Iosevka Comfy")
	  (cascadia
	   :default-family "Cascadia Mono"
	   :default-weight semilight)
	  (iosvmata
	   :default-family "Iosvmata")
	  (nrk
	   :default-family "NRK Mono")
	  (pragmasevka
	   :default-family "Pragmasevka"
	   :line-spacing 3
	   :default-weight regular
	   :bold-weight extrabold)))
  (fontaine-set-preset 'iosvmata))
(use-package gruber-darker-theme
  :disabled
  :config
  (disable-theme 'greenized)
  (load-theme 'gruber-darker t))
(use-package solarized-theme
  :config
  (disable-theme 'greenized)
  (load-theme 'solarized-dark))
(use-package smart-mode-line
  :config
  (sml/setup)
  (setq sml/theme 'respectful)
  (smart-mode-line-enable 1))

;; M-w
(use-package easy-kill
  :config
  (global-set-key [remap kill-ring-save] 'easy-kill))

;; icons
(use-package all-the-icons
  :custom (all-the-icons-scale-factor 0.9))
(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))
(use-package all-the-icons-completion
  :after (marginalia all-the-icons)
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup)
  :init (all-the-icons-completion-mode))

;; dired
(use-package dired-hide-dotfiles
  :hook (dired-mode . dired-hide-dotfiles-mode)
  :bind
  (:map dired-mode-map
	("h" . dired-hide-dotfiles-mode)))

;; extra modes
(use-package cmake-mode :defer t)
(use-package i3wm-config-mode :defer t)
(use-package lua-mode :defer t)
(use-package markdown-mode :defer t)
(use-package pdf-tools :defer t)
(use-package web-mode :defer t)
(use-package yaml-mode :defer t)
(use-package dockerfile-mode :defer t)
(use-package diminish
  :config
  (diminish 'company-mode)
  (diminish 'fancy-dabbrev-mode)
  (diminish 'completion-preview-mode)
  (diminish 'eldoc-mode))

(use-package cape
  :init
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-block))

(use-package fancy-dabbrev
  :init
  (global-fancy-dabbrev-mode)
  :config
  (setq dabbrev-case-distinction nil)
  (setq dabbrev-case-fold-search t)
  (setq dabbrev-case-replace nil)
  (global-set-key (kbd "TAB") 'fancy-dabbrev-expand-or-indent))

(use-package company
  :hook
  (after-init . global-company-mode)
  :custom
  (company-idle-delay nil)
  (company-minimum-prefix-length 1)
  :config
  (setq company-text-icons-add-background t)
  (setq company-tooltip-align-annotations t)
  (setq company-format-margin-function 'company-text-icons-margin)
  (setq company-frontends
	'(company-pseudo-tooltip-unless-just-one-frontend
	  company-echo-strip-common-frontend
	  company-preview-if-just-one-frontend))
  (setq-local completion-at-point-functions
	      (mapcar #'cape-company-to-capf
		      (list #'company-files #'company-keywords #'company-dabbrev)))
  (define-key company-mode-map (kbd "C-M-i") 'company-complete)
  (define-key company-active-map (kbd "C-M-i") 'company-complete))

;; show vertical lines
(use-package page-break-lines
  :diminish
  :config
  (global-page-break-lines-mode))

;; edit by sudo
(use-package sudo-edit :defer t)

;; save on focus loose
(use-package super-save
  :diminish
  :config
  (super-save-mode t))

;; buffer moviments
(use-package transpose-frame)
(use-package buffer-move
  :custom
  (buffer-move-behavior 'move)
  :bind
  (("<C-S-up>" . buf-move-up)
   ("<C-S-down>" . buf-move-down)
   ("<C-S-left>" . buf-move-left)
   ("<C-S-right>" . buf-move-right)))

;; search on file engine
(use-package ctrlf
  :config
  (ctrlf-mode t)
  :custom
  (ctrlf-auto-recenter t)
  (ctrlf-alternate-search-style 'literal))

;; clojure
(use-package cider
  :defer t
  :custom
  (nrepl-hide-special-buffers t)
  (nrepl-log-messages nil)
  (cider-font-lock-dynamically '(macro core function var deprecated))
  (cider-overlays-use-font-lock t)
  (cider-prompt-for-symbol nil)
  (cider-inject-dependencies-at-jack-in nil)
  (cider-repl-history-display-duplicates nil)
  (cider-repl-history-display-style 'one-line)
  (cider-repl-history-file (concat user-emacs-directory "cache/cider-repl-history"))
  (cider-repl-history-highlight-current-entry t)
  (cider-repl-history-quit-action 'delete-and-restore)
  (cider-repl-history-highlight-inserted-item t)
  (cider-repl-history-size 1000)
  (cider-repl-result-prefix ";; => ")
  (cider-repl-print-length 100)
  (cider-repl-use-clojure-font-lock t)
  (cider-repl-use-pretty-printing t)
  (cider-repl-wrap-history nil)
  (cider-stacktrace-default-filters '(tooling dup))
  (cider-repl-display-help-banner nil)
  (ider-repl-pop-to-buffer-on-connect 'display-only))

;; python
(use-package pytest
  :defer t)
(use-package virtualenvwrapper
  :config (venv-initialize-eshell))
(use-package electric-operator
  :diminish
  :hook
  (python-mode . electric-operator-mode))

;; organize cache files
(use-package no-littering
  :config
  (require 'no-littering)
  (savehist-mode))

;; show helper command
(use-package which-key
  :diminish
  :config
  (which-key-mode t))

;; undo tree visualization
(use-package vundo :defer t)
(use-package undo-fu-session
  :config
  (undo-fu-session-global-mode t)
    (when (executable-find "zstd")
    ;; There are other algorithms available, but zstd is the fastest, and speed
    ;; is our priority within Emacs
    (setq-default undo-fu-session-compression 'zst)))

;; setup fonts
(use-package mixed-pitch
  :custom
  (mixed-pitch-set-height t))

;; darker buffer where is not about edit text
;; (use-package solaire-mode
;;   :ensure t
;;   :hook
;;   (change-major-mode . turn-on-solaire-mode)
;;   (after-revert . turn-on-solaire-mode)
;;   (ediff-prepare-buffer . solaire-mode)
;;   :custom
;;   (solaire-mode-auto-swap-bg t)
;;   :config
;;   (solaire-global-mode +1))

(use-package expand-region
  :bind
  ("M-@" . er/expand-region))

;; move parens
(use-package smartparens
  :bind
  ("C-)" . sp-forward-slurp-sexp)
  ("C-(" . sp-forward-barf-sexp))

;; show flags
(use-package hl-todo
  :hook (prog-mode . hl-todo-mode)
  :hook (org-mode  . hl-todo-mode)
  :hook (yaml-mode . hl-todo-mode)
  :custom
  (hl-todo-keyword-faces
   '(("TODO" warning bold)
     ("FIXME" error bold)
     ("REVIEW" font-lock-keyword-face bold)
     ("HACK" font-lock-constant-face bold)
     ("DEPRECATED" font-lock-doc-face bold)
     ("NOTE" success bold)
     ("BUG" error bold)
     ("XXX" font-lock-constant-face bold))))

;; more cursor, mass editing
(use-package multiple-cursors
  :bind
  ("C->" . 'mc/mark-next-like-this)
  ("C-<" . 'mc/mark-previous-like-this)
  ("C-c C-<" . 'mc/edit-lines)
  ("C-c C->" . 'mc/mark-all-like-this)
  ("M-<mouse-1>" . mc/add-cursor-on-click))

;; (use-package winner
;;   :preface
;;   (defvar winner-dont-bind-my-keys t) ; I'll bind keys myself
;;   :hook
;;   (dashboard-setup-startup . winner-mode)
;;   :config
;;   (winner-mode 1)
;;   (appendq! winner-boring-buffers
;;	    '("*Compile-Log*" "*inferior-lisp*" "*Fuzzy Completions*"
;;	      "*Apropos*" "*Help*" "*cvs*" "*Buffer List*" "*Ibuffer*"
;;	      "*esh command on file*")))

(use-package rainbow-delimiters
  :bind
  ("<f8>" . rainbow-delimiters-mode)
  ;; :config
  ;; (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
  :custom
  (rainbow-delimiters-max-face-count 4))

;; UUID
(use-package uuidgen :defer t)

;; ;; git!
(use-package git-link :defer t)
(use-package git-timemachine :defer t)
(use-package magit :defer t)

;; move blocks
(use-package drag-stuff
  :bind
  (("<M-up>" . drag-stuff-up)
   ("<M-down>" . drag-stuff-down)
   ("<M-left>" . drag-stuff-left)
   ("<M-right>". drag-stuff-right)))

;; goto reference engine withtou LSP
(use-package dumb-jump
  :init
  :custom
  (dumb-jump-default-project (concat user-emacs-directory "cache/jump"))
  (dumb-jump-prefer-searcher 'rg)
  (dumb-jump-aggressive nil)
  (dumb-jump-selector 'ivy)
  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  (add-hook 'dumb-jump-after-jump-hook #'better-jumper-set-jump))

(use-package popwin
  :custom
  (popwin:popup-window-width 0.4)
  (popwin:popup-window-position 'right)
  :config
  (popwin-mode t))

(use-package nvm
  :config
  (when (file-exists-p "~/.nvmrc")
    (nvm-use-for)))

(use-package helpful
  :bind
  (("C-h f" . helpful-callable)
   ("C-h v" . helpful-variable)
   ("C-h k" . helpful-key)
   ("C-h x" . helpful-command)
   ("C-c C-d" . help-at-point)
   ("C-h F" . helpful-function))
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable))

;; edit all references
(use-package iedit
  :config
  (require 'iedit))

;; textual replacement of gui questions
(use-package ace-popup-menu
  :config
  (ace-popup-menu-mode t))

;; pulsar cursor
(use-package pulsar
  :custom
  (pulsar-pulse t)
  :config
  (defun pulse-line (&rest _)
    (pulsar-pulse-line))
  (dolist (command '(scroll-up-command
		     scroll-down-command
		     recenter-top-bottom
		     windmove-right
		     windmove-left
		     windmove-up
		     windmove-down
		     other-window
		     ace-window))
    (advice-add command :after #'pulse-line)))

;; avoid trash in kill ring
(use-package clean-kill-ring
  :config (clean-kill-ring-mode))

;; show usefull information at left on minibuffer
(use-package marginalia
  :custom
  (marginalia-align 'left)
  :init
  (marginalia-mode))

;; better minibuffer
(use-package consult
  :config (setq completion-in-region-function
		(lambda (&rest args)
		  (apply (if vertico-mode
			     #'consult-completion-in-region
			   #'completion--in-region)
			 args)))
  (setq xref-show-xrefs-function #'consult-xref
	xref-show-definitions-function #'consult-xref)
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :bind (("M-y"     . consult-yank-pop)
	 ("C-x b"   . consult-buffer)
	 ("C-c m"   . consult-global-mark)
	 ("C-c f"   . consult-find)
	 ("C-c i"   . consult-imenu)
	 ("C-c r"   . consult-ripgrep)
	 ("C-c l"   . consult-line-multi)
	 ("M-g g"   . consult-goto-line)
	 ("M-g M-g" . consult-goto-line)))

;; improve icomplete
(use-package orderless
  :custom
  (completion-styles '(orderless flex))
  (completion-category-overrides '((file (styles basic partial-completion)))))

;; minibuffer search candidate
(use-package vertico
  :config
  (vertico-mode t))

;; html genereta tags html>body
(use-package emmet-mode
  :after web-mode
  :hook
  (web-mode . emmet-mode))

;; show color hex with background color
(use-package rainbow-mode
  :diminish
  :hook
  (prog-mode . rainbow-mode))

;; (use-package fancy-compilation
;;   :custom
;;   (fancy-compilation-override-colors nil)
;;   :config
;;   (fancy-compilation-mode))

;; move open buffers
(use-package ace-window
  :bind
  ("M-o" . ace-window))

;; Blink mode line
(use-package mode-line-bell
  :config
  (mode-line-bell-mode t))

;; Error ui
;; (use-package flymake)
;; (use-package sideline
;;   :hook (flycheck-mode . sideline-mode)
;;   :custom
;;   (sideline-backends-right '(sideline-flycheck)))
;; (use-package sideline-flymake
;;   :hook (flymake-mode . sideline-mode)
;;   :init
;;   (setq sideline-flymake-display-mode 'point)
;;   (setq sideline-backends-right '(sideline-flymake)))

;; Jump to char
(use-package avy
  :custom
  (avy-background t)
  (avy-single-candidate-jump t)
  :config
  (global-set-key (kbd "M-z") 'avy-goto-word-1))

;; LSP
(use-package eglot
  :hook
  (prog-mode-hook . eglot-ensure)
  :custom
  (defalias 'start-lsp-server #'eglot)
  (eglot-autoshutdown t)
  (eglot-sync-connect t)
  (eglot-connect-timeout 3)
  (eglot-events-buffer-size 0)
  :config
  (setq eglot-ignored-server-capabilities '(:inlayHintProvider
					    :foldingRangeProvider
					    :hoverProvider))
  (setq eglot-report-progress nil))
;; (when (executable-find "emacs-lsp-booster")
;;   (use-package eglot-booster
;;     :ensure
;;     :quelpa ((eglot-booster :fetcher github :repo "jdtsmith/eglot-booster/" :file "eglot-booster") :upgrade t))
;;     :config (eglot-booster-mode))

;; Overlay symbol!!
(use-package symbol-overlay
  :diminish
  :custom
  (symbol-overlay-idle-time 0.5)
  (symbol-overlay-temp-highlight-single nil)
  :hook
  (prog-mode . symbol-overlay-mode))

;; List of coletions
(use-package embark
  :bind
  ("C-." . embark-act)
  :custom
  (prefix-help-command #'embark-prefix-help-command)
  :config
  (add-to-list 'display-buffer-alist
	       '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
		 nil
		 (window-parameters (mode-line-format . none)))))

;; org mode
(use-package org-modern
  :hook
  (org-mode . org-modern-mode)
  :custom
  (org-modern-fold-stars
   '(("▶" . "▼") ("▷▷" . "▽▽") ("⯈⯈⯈" . "⯆⯆⯆") ("▹▹▹▹" . "▿▿▿▿") ("▸▸▸▸▸" . "▾▾▾▾▾")))
  :config
  (with-eval-after-load 'org (global-org-modern-mode)))
(use-package verb
  :defer t
  :config (define-key org-mode-map (kbd "C-c C-r") verb-command-map))

;; libterm terminal
(use-package vterm :defer t)
(use-package shell-pop
  :defer t
  :custom
  (shell-pop-shell-type (quote ("ansi-term" "*ansi-term*" (lambda nil (ansi-term shell-pop-term-shell)))))
  (shell-pop-universal-key "<f12>")
  (shell-pop-window-size 30)
  (shell-pop-full-span t)
  (shell-pop-window-position "bottom")
  (shell-pop-restore-window-configuration t))

(use-package doom-modeline
  :disabled
  :custom
  (doom-modeline-major-mode-color-icon nil)
  (doom-modeline-buffer-file-name-style 'truncate-upto-root)
  (doom-modeline-buffer-modification-icon t)
  (doom-modeline-time-icon t)
  (doom-modeline-icon t)
  (doom-modeline-highlight-modified-buffer-name t)
  (doom-modeline-position-column-line-format '("(%l:%c)"))
  (doom-modeline-enable-word-count t)
  (doom-modeline-height 28)
  (doom-modeline-buffer-encoding nil)
  :init
  (doom-modeline-mode 1))

(use-package breadcrumb
  :disabled
  :init
  (breadcrumb-mode 0))

(use-package yasnippet
  :init
  (yas-global-mode 1))

(use-package scroll-on-jump
  :custom
  (scroll-on-jump-curve 'linear)
  :config
  (scroll-on-jump-advice-add forward-paragraph)
  (scroll-on-jump-advice-add backward-paragraph))

;; RSS feed
(use-package elfeed :defer t
  :custom
  (elfeed-feeds
   '(("https://www.redblobgames.com/blog/posts.xml" gamedev programming)
     ("https://eev.ee/feeds/blog.atom.xml" gamedev)
     ("http://feeds.feedburner.com/Blog-Slynyrd" gamedev art)
     )))
(use-package elfeed-org :after elfeed :defer t)

(provide 'init-packages)
;;; init.el ends here
