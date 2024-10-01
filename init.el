(when (version< emacs-version "29.0")
  (message "Your Emacs is old. Please upgrade if possible."))

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(setq custom-file (concat user-emacs-directory "custom.el"))
(when (file-exists-p custom-file)
  (load-file custom-file))

(defalias 'yes-or-no-p 'y-or-n-p)

(fringe-mode 10)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode -1)

(column-number-mode 1)
(global-display-line-numbers-mode 1)
(context-menu-mode 1)
(repeat-mode 1)
(save-place-mode 1)
(savehist-mode 1)
(windmove-default-keybindings)

(require 'uniquify)

(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "M-!")     'async-shell-command)
(global-set-key (kbd "M-&")     'shell-command)
(global-set-key (kbd "M-u")     'upcase-dwim)
(global-set-key (kbd "M-l")     'downcase-dwim)
(global-set-key (kbd "M-c")     'capitalize-dwim)
(global-set-key (kbd "C-x C-d") 'dired)
(global-set-key (kbd "C-,")     'duplicate-line)

;; 3party
(use-package dumb-jump
  :custom
  (dumb-jump-prefer-searcher 'rg)
  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  (add-hook 'dumb-jump-after-jump-hook #'better-jumper-set-jump))
(use-package orderless
  :init
  (require 'orderless)
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))
(use-package mct
  :init
  (mct-mode 1))
(use-package solaire-mode
  :init
  (solaire-global-mode 1))
(use-package ctrlf
  :init
  (ctrlf-mode t))
(use-package undo-fu-session
  :init
  (undo-fu-session-global-mode 1))
(use-package rainbow-delimiters
  :hook
  (prog-mode . rainbow-delimiters-mode))
(use-package org-modern
  :hook
  (org-mode . org-modern-mode)
  :custom
  (org-modern-fold-stars
   '(("▶" . "▼")
     ("▷▷" . "▽▽")
     ("⯈⯈⯈" . "⯆⯆⯆")
     ("▹▹▹▹" . "▿▿▿▿")
     ("▸▸▸▸▸" . "▾▾▾▾▾"))))
(use-package consult
  :init
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
(use-package virtualenvwrapper
  :config (venv-initialize-eshell))
