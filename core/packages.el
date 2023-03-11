(require 'cl-lib)

(defvar packages-list-p
  '(ace-window
    ag
    browse-kill-ring
    diff-hl
    diminish
    discover-my-major
    dockerfile-mode
    easy-kill
    epl
    eglot
    elpy
    exec-path-from-shell
    expand-region
    flycheck
    gist
    git-modes
    git-timemachine
    imenu-anywhere
    json-mode
    magit
    operate-on-number
    restclient
    smartparens
    smartrep
    super-save
    undo-tree
    which-key
    whitespace-cleanup-mode
    zop-to-char))


(defun packages-installed-p ()
  (cl-every #'package-installed-p packages-list-p))

(defun require-package (package)
  (unless (memq package packages-list-p)
    (add-to-list 'packages-list-p package))
  (unless (package-installed-p package)
    (package-install package)))

(defun require-packages (packages)
  (mapc #'require-package packages))

(defun install-packages ()
  (unless (packages-installed-p)
    (message "%s" "Emacs is now refreshing its package database...")
    (package-refresh-contents)
    (message "%s" " done.")
    (require-packages packages-list-p)))

;; run package installation
(install-packages)
;; ----------------------------------

(use-package bookmark
  :config
  (setq bookmark-default-file (expand-file-name "bookmarks" savefile-dir)
        bookmark-save-flag 1))

(use-package projectile
  :config
  (setq projectile-cache-file (expand-file-name  "projectile.cache" savefile-dir))
  (projectile-mode t))

(use-package avy
  :custom
  (avy-background t))

(use-package expand-region
  :bind ("M-@" . er/expand-region))

(use-package goto-last-change
  :bind ("C-:" . goto-last-change))


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


(use-package highlight-thing
  :init
  (global-highlight-thing-mode t))

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

(use-package consult
  :bind
  (("C-x b" . consult-buffer)
   ("C-s" . consult-line)
   :map minibuffer-local-map
   ("C-r" . consult-history))
  :custom
  (completion-in-region-function #'consult-completion-in-region))

(use-package diff-hl
  :init
  (global-diff-hl-mode))

(use-package whitespace-cleanup-mode
  :init
  (global-whitespace-cleanup-mode))

(use-package mwim
  :bind
  ("C-a" . mwim-beginning)
  ("C-e" . mwim-end))


(provide 'packages)
