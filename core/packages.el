(require 'cl-lib)

(defvar packages-list-p
  '(ace-window
    ag
    all-the-icons
    cider
    clang-format
    diminish
    expand-region
    elpy
    git-modes
    git-timemachine
    git-link
    magit
    restclient
    sudo-edit
    use-package-ensure-system-package
    ;; which-key
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

(install-packages)
;; ----------------------------------

(defmacro auto-install (extension package mode)
  `(add-to-list 'auto-mode-alist
                `(,extension . (lambda ()
                                 (unless (package-installed-p ',package)
                                   (package-install ',package))
                                 (,mode)))))


(defvar auto-install-alist
  '(("\\.adoc\\'" adoc-mode adoc-mode)
    ("\\.clj\\'" clojure-mode clojure-mode)
    ("\\.cljc\\'" clojure-mode clojurec-mode)
    ("\\.cljs\\'" clojure-mode clojurescript-mode)
    ("\\.edn\\'" clojure-mode clojure-mode)
    ("\\.cmake\\'" cmake-mode cmake-mode)
    ("CMakeLists\\.txt\\'" cmake-mode cmake-mode)
    ("\\.coffee\\'" coffee-mode coffee-mode)
    ("\\.css\\'" css-mode css-mode)
    ("\\.css\\'" scss-mode scss-mode)
    ("\\.csv\\'" csv-mode csv-mode)
    ("Cask" cask-mode cask-mode)
    ("\\.d\\'" d-mode d-mode)
    ("\\.dart\\'" dart-mode dart-mode)
    ("\\.elm\\'" elm-mode elm-mode)
    ("\\.ex\\'" elixir-mode elixir-mode)
    ("\\.exs\\'" elixir-mode elixir-mode)
    ("\\.elixir\\'" elixir-mode elixir-mode)
    ("\\.erl\\'" erlang erlang-mode)
    ("\\.feature\\'" feature-mode feature-mode)
    ("\\.go\\'" go-mode go-mode)
    ("\\.graphql\\'" graphql-mode graphql-mode)
    ("\\.groovy\\'" groovy-mode groovy-mode)
    ("\\.haml\\'" haml-mode haml-mode)
    ("\\.hs\\'" haskell-mode haskell-mode)
    ("\\.jl\\'" julia-mode julia-mode)
    ("\\.json\\'" json-mode json-mode)
    ("\\.kt\\'" kotlin-mode kotlin-mode)
    ("\\.kv\\'" kivy-mode kivy-mode)
    ("\\.latex\\'" auctex LaTeX-mode)
    ("\\.less\\'" less-css-mode less-css-mode)
    ("\\.lua\\'" lua-mode lua-mode)
    ("\\.markdown\\'" markdown-mode markdown-mode)
    ("\\.md\\'" markdown-mode markdown-mode)
    ("\\.ml\\'" tuareg tuareg-mode)
    ("\\.pp\\'" puppet-mode puppet-mode)
    ("\\.php\\'" php-mode php-mode)
    ("\\.proto\\'" protobuf-mode protobuf-mode)
    ("\\.pyd\\'" cython-mode cython-mode)
    ("\\.pyi\\'" cython-mode cython-mode)
    ("\\.pyx\\'" cython-mode cython-mode)
    ("PKGBUILD\\'" pkgbuild-mode pkgbuild-mode)
    ("\\.rkt\\'" racket-mode racket-mode)
    ("\\.rs\\'" rust-mode rust-mode)
    ("\\.shader\\'" shader-mode shader-mode)
    ("\\.sass\\'" sass-mode sass-mode)
    ("\\.scala\\'" scala-mode scala-mode)
    ("\\.scss\\'" scss-mode scss-mode)
    ("\\.slim\\'" slim-mode slim-mode)
    ("\\.styl\\'" stylus-mode stylus-mode)
    ("\\.swift\\'" swift-mode swift-mode)
    ("\\.textile\\'" textile-mode textile-mode)
    ("\\.thrift\\'" thrift thrift-mode)
    ("\\.yml\\'" yaml-mode yaml-mode)
    ("\\.yaml\\'" yaml-mode yaml-mode)
    ("Dockerfile\\'" dockerfile-mode dockerfile-mode)))

;; build auto-install mappings
(mapc
 (lambda (entry)
   (let ((extension (car entry))
         (package (cadr entry))
         (mode (cadr (cdr entry))))
     (unless (package-installed-p package)
       (auto-install extension package mode))))
 auto-install-alist)

;; ----------------------------------

;; todo add package to mode
(use-package auto-compile
  :config
  (auto-compile-on-load-mode)
  (auto-compile-on-save-mode))

(use-package projectile
  :bind
  ("<f9>" . projectile-compile-project)
  :config
  (setq projectile-cache-file (expand-file-name  "projectile.cache" savefile-dir))
  (projectile-mode t))

(use-package expand-region
  :bind ("M-@" . er/expand-region))

(use-package company
  :diminish
  :config
  (global-company-mode t))

(use-package vertico
  :custom
  (vertico-cycle t)
  :init
  (vertico-mode t))

(use-package marginalia
  :after vertico
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :init
  (marginalia-mode t))

(use-package consult
  :bind
  ("C-x b" . consult-buffer)
  ("C-s" . swiper)
  :custom
  (completion-in-region-function #'consult-completion-in-region))

(use-package amx
  :config
  (amx-mode))

(use-package mwim
  :bind
  ("C-a" . mwim-beginning)
  ("C-e" . mwim-end))

(use-package counsel
  :commands (counsel-yank-pop counsel-ag counsel-fzf)
  :bind
  ("M-?" . counsel-ag)
  ("C-M-?" . counsel-fzf))

(use-package counsel-projectile
  :bind
  ("M-?" . counsel-projectile-ag)
  ("C-M-?" . counsel-projectile-find-file))

(use-package undo-tree
  :diminish
  :init
  (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))
  (global-undo-tree-mode))

(use-package git-gutter
  :diminish
  :hook (prog-mode . git-gutter-mode)
  :custom
  (git-gutter:modified-sign ">")
  (git-gutter:added-sign "+")
  (git-gutter:deleted-sign "-")
  (git-gutter:update-interval 0.02))

(use-package super-save
  :diminish
  :config
  (super-save-mode t))

(use-package flycheck
  :config
  ;; (setq flycheck-check-syntax-automatically '(save mode-enable))
  (global-flycheck-mode))

(use-package dumb-jump
  :hook
  (xref-backend-functions dumb-jump-xref-activate))

(use-package smartparens
  :bind
  ("C-)" . sp-forward-slurp-sexp)
  ("C-(" . sp-forward-barf-sexp))

(use-package exec-path-from-shell
  :init
  (exec-path-from-shell-initialize))

(use-package multiple-cursors
  :config
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this))

(use-package highlight-numbers
  :config
  (add-hook 'prog-mode-hook 'highlight-numbers-mode))

(use-package fancy-dabbrev
  :diminish
  :config
  (global-fancy-dabbrev-mode)
  (global-set-key (kbd "TAB") 'fancy-dabbrev-expand-or-indent)
  (global-set-key (kbd "<backtab>") 'fancy-dabbrev-backward)
  (setq dabbrev-case-distinction nil)
  (setq dabbrev-case-fold-search t)
  (setq dabbrev-case-replace nil))

(use-package dired-k
  :config
  (add-hook 'dired-initial-position-hook 'dired-k))

(use-package smart-compile)

(use-package prescient)
(use-package vertico-prescient)

(use-package good-scroll
  :config
  (good-scroll-mode 1))

(use-package fussy
  :config
  (push 'fussy completion-styles))

(use-package gcmh
  :diminish
  :config
  (gcmh-mode t))

(use-package eglot
  :hook
  (eglot-managed-mode-hook . eglot-inlay-hints-mode)
  (prog-mode . eglot-ensure))

(use-package iedit
  :config
  (require 'iedit))

(use-package browse-kill-ring
  :bind
  ("M-y" . browse-kill-ring))

(use-package pulsar
  :hook
  (minibuffer-setup-hook . pulsar-pulse-line)
  (consult-after-jump-hook . pulsar-recenter-top)
  (consult-after-jump-hook . pulsar-reveal-entry)
  (imenu-after-jump-hook . pulsar-recenter-top)
  (imenu-after-jump-hook . pulsar-reveal-entry)
  :custom
  (pulsar-pulse t)
  :config
  (pulsar-global-mode t))

(if (>= emacs-major-version 29)
    (pixel-scroll-precision-mode t)
  (use-package good-scroll
    :config
    (good-scroll-mode t)))

(use-package mode-line-bell
  :config
  (mode-line-bell-mode))

(use-package whitespace-cleanup-mode
  :diminish
  :config
  (global-whitespace-cleanup-mode))

(use-package eldoc-box
  :config
  (setq eldoc-box-offset '(16 32 16))
  (setq eldoc-box-max-pixel-width 400)
  (setq eldoc-box-max-pixel-height 500)
  (add-hook 'eglot-managed-mode-hook #'eldoc-box-hover-mode t)
  (eldoc-box-hover-mode t))

(use-package cyphejor
  :config
  (setq
   cyphejor-rules
   '(:upcase
     ("bookmark"    "→")
     ("buffer"      "β")
     ("diff"        "Δ")
     ("dired"       "δ")
     ("emacs"       "ε")
     ("lisp"        "λ" :postfix)
     ("menu"        "▤" :postfix)
     ("mode"        "")
     ("package"     "↓")
     ("python"      "π")
     ("shell"       "sh" :postfix)
     ("text"        "ξ")
     ("wdired"      "↯δ")))
  (cyphejor-mode t))

(provide 'packages)
