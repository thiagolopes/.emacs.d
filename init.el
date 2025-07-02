(when (version< emacs-version "29.0")
  (message "Your Emacs is old. Please upgrade if possible."))

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(setq custom-file (concat user-emacs-directory "custom.el"))
(when (file-exists-p custom-file)
  (load-file custom-file))

(defun my-pckgs-not-installed ()
  (when (not (package-installed-p 'dash))
    (package-install 'dash))
  (require 'dash)
  (-filter
   (-not #'package-installed-p)
   package-selected-packages))

(let ((packages-not-installed (my-pckgs-not-installed)))
  (when packages-not-installed
    (-map #'package-install packages-not-installed)
    (restart-emacs) ;; OVERKILL
    ))

(defalias 'yes-or-no-p 'y-or-n-p)

;; this disable scroll on minibuffer
;; (set-window-scroll-bars (minibuffer-window) nil nil nil nil 1)

(fringe-mode 10)
(tool-bar-mode -1)
(menu-bar-mode -1)
(blink-cursor-mode -1)
(scroll-bar-mode -1)

(column-number-mode 1)
(context-menu-mode 1)
(repeat-mode 1)
(winner-mode 1)
(save-place-mode 1)
(savehist-mode 1)
(windmove-default-keybindings)
(delete-selection-mode 1)
(show-paren-mode 1)
(prefer-coding-system 'utf-8)
(global-auto-revert-mode 1)

;; default size
(add-to-list 'default-frame-alist '(height . 35))
(add-to-list 'default-frame-alist '(width . 160))

;; use .emacs.d/backup to store backup, WARNING storing senvitive data.
(let ((temporary-file-directory (expand-file-name
                                 (convert-standard-filename "backup/")
                                 user-emacs-directory)))
  (unless (file-exists-p temporary-file-directory)
    (make-directory temporary-file-directory))
  (setq backup-directory-alist
        `((".*" . ,temporary-file-directory)))
  (setq auto-save-file-name-transforms
        `((".*" ,temporary-file-directory t))))

(require 'uniquify)

(global-set-key (kbd "C-x k")       'kill-current-buffer)
(global-set-key (kbd "C-x C-b")     'ibuffer)
(global-set-key (kbd "M-!")         'async-shell-command)
(global-set-key (kbd "M-&")         'shell-command)
(global-set-key (kbd "M-u")         'upcase-dwim)
(global-set-key (kbd "M-l")         'downcase-dwim)
(global-set-key (kbd "M-c")         'capitalize-dwim)
(global-set-key (kbd "C-x C-d")     'dired)
(global-set-key (kbd "C-,")         'duplicate-line)
(global-set-key (kbd "<f9>")        'project-compile)
(global-set-key (kbd "S-C-<left>")  'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>")  'shrink-window)
(global-set-key (kbd "S-C-<up>")    'enlarge-window)

(setq-default mode-line-format
      '("%e" mode-line-front-space
        (:propertize
         ("" mode-line-mule-info
          mode-line-client
          mode-line-modified
          mode-line-remote
          mode-line-window-dedicated)
         display (min-width (2.0)))
        mode-line-frame-identification
        mode-line-buffer-identification
        "   "
        mode-line-position
        "   ("(project-mode-line project-mode-line-format)")"
        "    /"(vc-mode vc-mode)"/ "
        "  "
        mode-line-modes mode-line-misc-info
        mode-line-end-spaces
        mode-line-format-right-align
        "["mode-line-percent-position"]       "
        ))

;; 3party
(global-set-key [remap goto-line] 'goto-line-preview)
(global-set-key (kbd "C-x /")     'goto-last-change)
(require 'no-littering)

(use-package mode-line-bell
  :config
  (mode-line-bell-mode))

(use-package dumb-jump
  :custom
  (dumb-jump-prefer-searcher 'rg)
  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  (add-hook 'dumb-jump-after-jump-hook 'better-jumper-set-jump))

(use-package rainbow-delimiters
  :bind
  ("<f5>" . rainbow-delimiters-mode))

(use-package org-modern
  :hook
  (org-mode . org-modern-mode)
  (org-mode . visual-line-mode)
  (org-mode . variable-pitch-mode)
  :custom
  (org-modern-fold-stars
   '(("▶" . "▼")
     ("▷▷" . "▽▽")
     ("⯈⯈⯈" . "⯆⯆⯆")
     ("▹▹▹▹" . "▿▿▿▿")
     ("▸▸▸▸▸" . "▾▾▾▾▾"))))

(use-package consult
  :custom
  (xref-show-definitions-function #'consult-xref)
  (xref-show-xrefs-function #'consult-xref)
  :hook
  (completion-list-mode . consult-preview-at-point-mode)
  :bind (("M-y"     . consult-yank-pop)
         ("C-x b"   . consult-buffer)
         ("C-c p"   . consult-project-buffer)
         ("C-c m"   . consult-mark)
         ("C-c f"   . consult-find)
         ("C-c i"   . consult-imenu)
         ("C-c r"   . consult-ripgrep)
         ("C-c l"   . consult-line-multi)
         ("M-g g"   . consult-goto-line)
         ("M-g M-g" . consult-goto-line)))

(use-package multiple-cursors
  :bind
  ("C->" . 'mc/mark-next-like-this)
  ("C-<" . 'mc/mark-previous-like-this)
  ("C-c C-<" . 'mc/edit-lines)
  ("C-c C->" . 'mc/mark-all-like-this))

(use-package expand-region
  :bind
  ("M-@" . er/expand-region))

(use-package mwim
  :bind (("C-a" . mwim-beginning)
         ("C-e" . mwim-end)))

(use-package hledger-mode
  :mode ("\\.journal\\'" . hledger-mode)
  :bind (:map hledger-mode-map
              ("<f9>" . hledger-run-command))
  :config
  (add-hook 'hledger-mode-hook (lambda ()
                                 (whitespace-mode))))

(use-package highlight-numbers
  :config
  (add-hook 'prog-mode-hook 'highlight-numbers-mode))

(use-package popwin
  :init
  (popwin-mode 1))

(use-package helpful
  :bind
  ("C-h f" . helpful-callable)
  ("C-h v" . helpful-variable)
  ("C-h k" . helpful-key)
  ("C-h x" . helpful-command))

(use-package vertico
  :init
  (vertico-mode 1))

(use-package marginalia
  :init
  (marginalia-mode 1))

(use-package treemacs
  :custom
  (treemacs-position 'right)
  (treemacs-deferred-git-apply-delay 5)
  :config
  (treemacs-fringe-indicator-mode 'only-when-focused)
  (treemacs-follow-mode t)
  (treemacs-git-mode 'simple)
  (treemacs-tag-follow-mode t)
  (treemacs-project-follow-mode t)
  :bind
  ("C-c t" . treemacs))

(use-package corfu
  :init
  (global-corfu-mode t)
  (corfu-history-mode)
  (corfu-popupinfo-mode)
  :config
  ;; Option 1: Unbind RET completely
  (keymap-unset corfu-map "RET")
  :custom
  (corfu-auto t)
  (corfu-preselect 'directory)
  :bind (:map corfu-map
              ("C-n" . corfu-next)
              ("C-p" . corfu-previous)
              ("<escape>" . corfu-quit)
              ;; ("<return>" . corfu-insert)
              ("M-d" . corfu-show-documentation)
              ("M-l" . corfu-show-location)))

(use-package dabbrev
  ;; Swap M-/ and C-M-/
  :bind (("M-/" . dabbrev-completion)
         ("C-M-/" . dabbrev-expand))
  :config
  (add-to-list 'dabbrev-ignored-buffer-regexps "\\` ")
  ;; Since 29.1, use `dabbrev-ignored-buffer-regexps' on older.
  (add-to-list 'dabbrev-ignored-buffer-modes 'doc-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'pdf-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'tags-table-mode))

(use-package kind-icon
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package ctrlf
  :bind ("M-s" . ctrlf-forward-symbol-at-point)
  :custom
  (ctrlf-alternate-search-style 'literal)
  (ctrlf-default-search-style 'regexp)
  :init
  (ctrlf-mode +1))

(use-package visual-fill-column
  :custom
  (visual-fill-column-center-text t)
  (visual-fill-column-width 90)
  :hook
  (org-mode . (lambda ()
                (setq-local whitespace-style '(face newline-mark))
                (setq-local whitespace-display-mappings
                            '((newline-mark ?\n [?↵ ?\n])))
                (whitespace-mode 1)))
  (org-mode . visual-fill-column-mode))

(use-package treesit-auto
  :custom
  (treesit-auto-install t)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))
