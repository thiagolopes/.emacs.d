(require 'cl-lib)

(defvar packages-list-p
  '(ace-window
    ag
    all-the-icons
    browse-kill-ring
    cider
    clang-format
    diminish
    discover-my-major
    expand-region
    gist
    git-modes
    git-timemachine
    iedit
    magit
    operate-on-number
    restclient
    smartrep
    use-package-ensure-system-package
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
(use-package elpy
  :init
  (elpy-enable))

(use-package projectile
  :bind
  ("<f9>" . projectile-compile-project)
  :config
  (setq projectile-cache-file (expand-file-name  "projectile.cache" savefile-dir))
  (projectile-mode t))

(use-package expand-region
  :bind ("M-@" . er/expand-region))

(use-package goto-last-change
  :bind ("C-:" . goto-last-change))

(use-package company
  :bind
  (:map  company-active-map ("<tab>" . company-complete-selection))
  :config
  (global-company-mode t))

(use-package highlight-thing
  :disabled
  :diminish
  :init
  (setq highlight-thing-case-sensitive-p t)
  (setq highlight-thing-exclude-thing-under-point t)
  (setq highlight-thing-delay-seconds 2)
  (setq highlight-thing-limit-to-region-in-large-buffers-p nil
        highlight-thing-narrow-region-lines 15
        highlight-thing-large-buffer-limit 5000)
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

(use-package consult
  :bind
  ("C-x b" . consult-buffer)
  ("C-s" . swiper)
  :custom
  (completion-in-region-function #'consult-completion-in-region))

(use-package amx
  :config
  (amx-mode))

(use-package popup-kill-ring)

(use-package key-chord
  :disabled
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

(use-package whitespace-cleanup-mode
  :diminish
  :init
  (global-whitespace-cleanup-mode))

(use-package mwim
  :bind
  ("C-a" . mwim-beginning)
  ("C-e" . mwim-end))

(use-package smart-mode-line
  :disabled
  :config
  (setq sml/no-confirm-load-theme t)
  (setq sml/theme 'respectful)
  (sml/setup))

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
  :hook
  (prog-mode . smartparens-mode)
  :bind
  ("C-)" . sp-forward-slurp-sexp)
  ("C-(" . sp-forward-barf-sexp))

(use-package exec-path-from-shell
  :init
  (exec-path-from-shell-initialize))

(use-package switch-window
  :config
  (global-set-key (kbd "C-x o") 'switch-window)
  (global-set-key (kbd "C-x 1") 'switch-window-then-maximize)
  (global-set-key (kbd "C-x 2") 'switch-window-then-split-below)
  (global-set-key (kbd "C-x 3") 'switch-window-then-split-right)
  (global-set-key (kbd "C-x 0") 'switch-window-then-delete)
  (global-set-key (kbd "C-x 4 d") 'switch-window-then-dired)
  (global-set-key (kbd "C-x 4 f") 'switch-window-then-find-file)
  (global-set-key (kbd "C-x 4 m") 'switch-window-then-compose-mail)
  (global-set-key (kbd "C-x 4 r") 'switch-window-then-find-file-read-only)
  (global-set-key (kbd "C-x 4 C-f") 'switch-window-then-find-file)
  (global-set-key (kbd "C-x 4 C-o") 'switch-window-then-display-buffer)
  (global-set-key (kbd "C-x 4 0") 'switch-window-then-kill-buffer)
  :init
  (setq switch-window-shortcut-appearance 'image))

(use-package multiple-cursors
  :config
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this))

(use-package highlight-numbers
  :config
  (add-hook 'prog-mode-hook 'highlight-numbers-mode))

(use-package fancy-dabbrev
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
  :config
  (gcmh-mode 1))

(use-package magit-delta
  :ensure-system-package (delta . "please, install delta")
  :hook (magit-mode . magit-delta-mode))

(use-package rainbow-delimiters
  :hook
  (prog-mode . rainbow-delimiters-mode))

(use-package eldoc-box
  :hook
  (prog-mode . eldoc-box-hover-at-point-mode))

(use-package eglot
  :hook (prog-mode . eglot-ensure))

(use-package iedit
  :config
  (require 'iedit))

(use-package cursory
  :config
  (setq cursory-presets
      '((bar
         :cursor-type (bar . 2)
         :cursor-in-non-selected-windows hollow
         :blink-cursor-blinks 10
         :blink-cursor-interval 0.4
         :blink-cursor-delay 0.2)
        (box
         :cursor-type box
         :cursor-in-non-selected-windows hollow
         :blink-cursor-blinks 10
         :blink-cursor-interval 0.5
         :blink-cursor-delay 0.2)
        (underscore
         :cursor-type (hbar . 3)
         :cursor-in-non-selected-windows hollow
         :blink-cursor-blinks 50
         :blink-cursor-interval 0.2
         :blink-cursor-delay 0.2)))
  (cursory-set-preset 'bar))

(use-package beacon
  :config
  (beacon-mode 1))

(provide 'packages)
