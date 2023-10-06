(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure 't)

(setq user-full-name "Thiago Lopes"
      user-mail-address "thiagolopes@protonmail.com")

(setq gc-cons-threshold 100000000
      read-process-output-max (* 1024 1024))

(custom-set-faces
 '(default ((t ( :weight normal :height 100 :width normal :family "Fira Code")))))

(setq whitespace-line-column nil
      whitespace-style
      '(face indentation tabs tab-mark spaces space-mark newline newline-mark
             trailing lines-tail)
      whitespace-display-mappings
      '((tab-mark ?\t [?› ?\t])
        (newline-mark ?\n [?¬ ?\n])
        (space-mark ?\  [?·] [?.])))

(setq dired-dwim-target t  ; suggest a target for moving/copying intelligently
      dired-hide-details-hide-symlink-targets nil
      ;; don't prompt to revert, just do it
      dired-auto-revert-buffer #'dired-buffer-stale-p
      ;; Always copy/delete recursively
      dired-recursive-copies  'always
      dired-recursive-deletes 'top
      ;; Ask whether destination dirs should get created when copying/removing files.
      dired-create-destination-dirs 'ask
      ;; Where to store image caches
      image-dired-dir (concat user-emacs-directory "image-dired/")
      image-dired-db-file (concat image-dired-dir "db.el")
      image-dired-gallery-dir (concat image-dired-dir "gallery/")
      image-dired-temp-image-file (concat image-dired-dir "temp-image")
      image-dired-temp-rotate-image-file (concat image-dired-dir "temp-rotate-image")
      ;; Screens are larger nowadays, we can afford slightly larger thumbnails
      image-dired-thumb-size 150)

(setq enable-recursive-minibuffers t)
;; Show current key-sequence in minibuffer ala 'set showcmd' in vim. Any
;; feedback after typing is better UX than no feedback at all.
(setq echo-keystrokes 0.02)
;; Expand the minibuffer to fit multi-line text displayed in the echo-area. This
;; doesn't look too great with direnv, however...
(setq resize-mini-windows 'grow-only)
;; Typing yes/no is obnoxious when y/n will do
(if (boundp 'use-short-answers)
    (setq use-short-answers t)
  ;; DEPRECATED: Remove when we drop 27.x support
  (advice-add #'yes-or-no-p :override #'y-or-n-p))
;; Try to keep the cursor out of the read-only portions of the minibuffer.
(setq minibuffer-prompt-properties '(read-only t intangible t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

(setq x-select-enable-clipboard-manager nil)
(setq warning-minimum-level :emergency)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(horizontal-scroll-bar-mode -1)
(menu-bar-mode -1)
(defalias 'yes-or-no-p 'y-or-n-p)
(show-paren-mode 1)
(setq-default indent-tabs-mode nil)
(setq save-interprogram-paste-before-kill t
      apropos-do-all t
      mouse-yank-at-point t
      require-final-newline t
      backup-by-copying t
      frame-inhibit-implied-resize t
      ediff-window-setup-function 'ediff-setup-windows-plain)

;; A simple frame title
(setq frame-title-format '("%b – Doom Emacs")
      icon-title-format frame-title-format)

;; Don't resize the frames in steps; it looks weird, especially in tiling window
;; managers, where it can leave unseemly gaps.
(setq frame-resize-pixelwise t)

;; But do not resize windows pixelwise, this can cause crashes in some cases
;; when resizing too many windows at once or rapidly.
(setq window-resize-pixelwise nil)

;; UX: GUIs are inconsistent across systems, desktop environments, and themes,
;;   and don't match the look of Emacs. They also impose inconsistent shortcut
;;   key paradigms. I'd rather Emacs be responsible for prompting.
(setq use-dialog-box nil)
(when (bound-and-true-p tooltip-mode)
  (tooltip-mode -1))

;; UX: Favor vertical splits over horizontal ones. Monitors are trending toward
;;   wide, rather than tall.
(setq split-width-threshold 160
      split-height-threshold nil)

;; FIX: The native border "consumes" a pixel of the fringe on righter-most
;;   splits, `window-divider' does not. Available since Emacs 25.1.
(setq window-divider-default-places t
      window-divider-default-bottom-width 1
      window-divider-default-right-width 1)
(add-hook 'prog-mode #'window-divider-mode)

;; Don't prompt for confirmation when we create a new file or buffer (assume the
;; user knows what they're doing).
(setq confirm-nonexistent-file-or-buffer nil)
(setq uniquify-buffer-name-style 'forward
      ;; no beeping or blinking please
      ring-bell-function #'ignore
      visible-bell nil)

;; middle-click paste at point, not at click
(setq mouse-yank-at-point t)

(setq hscroll-margin 2
      hscroll-step 1
      ;; Emacs spends too much effort recentering the screen if you scroll the
      ;; cursor more than N lines past window edges (where N is the settings of
      ;; `scroll-conservatively'). This is especially slow in larger files
      ;; during large-scale scrolling commands. If kept over 100, the window is
      ;; never automatically recentered. The default (0) triggers this too
      ;; aggressively, so I've set it to 10 to recenter if scrolling too far
      ;; off-screen.
      scroll-conservatively 10
      scroll-margin 0
      scroll-preserve-screen-position t
      ;; Reduce cursor lag by a tiny bit by not auto-adjusting `window-vscroll'
      ;; for tall lines.
      auto-window-vscroll nil
      ;; mouse
      mouse-wheel-scroll-amount '(2 ((shift) . hscroll))
      mouse-wheel-scroll-amount-horizontal 2)

(blink-cursor-mode -1)
(setq blink-matching-paren nil)
(setq x-stretch-cursor nil)
;; useful information, like git-gutter and flycheck.
(setq indicate-buffer-boundaries nil
      indicate-empty-lines nil)

(use-package doom-themes)
(load-theme 'doom-solarized-dark-high-contrast t)

;; Explicitly define a width to reduce the cost of on-the-fly computation
(setq-default display-line-numbers-width 3)
;; Show absolute line numbers for narrowed regions to make it easier to tell the
;; buffer is narrowed, and where you are, exactly.
(setq-default display-line-numbers-widen t)
;; Enable line numbers in most text-editing modes. We avoid
;; `global-display-line-numbers-mode' because there are many special and
;; temporary modes where we don't need/want them.
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(add-hook 'text-mode-hook #'display-line-numbers-mode)
(add-hook 'conf-mode-hook #'display-line-numbers-mode)

(electric-pair-mode t)

(setq display-line-numbers-type t
      org-directory "~/org/"
      cache-dir (concat user-emacs-directory "/cache")
      custom-file (concat user-emacs-directory "/custom.el")
      require-final-newline t
      load-prefer-newer t)

(defun map! (&rest key-func-pairs)
  (while key-func-pairs
    (let ((key (pop key-func-pairs))
          (func (pop key-func-pairs)))
      (global-set-key (kbd key) func))))

(map! "C-=" #'text-scale-increase
      "C-+" #'text-scale-increase
      "C--" #'text-scale-decrease
      "M-n" #'forward-paragraph
      "M-p" #'backward-paragraph)

(map! "<f3>" #'kmacro-start-macro-or-insert-counter
      "<f4>" #'kmacro-end-or-call-macro)

(map! "M-3" #'(lambda () (interactive) (insert "#"))
      "M-9" #'(lambda () (interactive) (insert "("))
      "M-0" #'(lambda () (interactive) (insert ")"))
      "M-[" #'(lambda () (interactive) (insert "{"))
      "M-]" #'(lambda () (interactive) (insert "}")))

(map! "C-x C-b" #'ibuffer
      "C-r" #'isearch-backward-regexp
      "C-M-s" #'isearch-forward
      "C-M-r" #'isearch-backward)

(map! "C-s" #'swiper-isearch-thing-at-point
      "C-S" #'swiper-isearch)

(use-package diminish)
(use-package expand-region
  :bind ("M-@" . er/expand-region))

(use-package counsel
  :commands (counsel-yank-pop counsel-ag counsel-fzf)
  :bind
  ("M-?" . counsel-ag)
  ("C-M-?" . counsel-fzf))

(use-package smartparens
  :bind
  ("C-)" . sp-forward-slurp-sexp)
  ("C-(" . sp-forward-barf-sexp))

(use-package dashboard
  :config
  (dashboard-setup-startup-hook))

(use-package cider)

(use-package company
  :diminish
  :hook
  (prog-mode . global-company-mode))

(use-package ligature
  :config
  (ligature-set-ligatures 'prog-mode '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
                                       ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
                                       "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
                                       "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
                                       "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
                                       "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
                                       "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
                                       "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
                                       ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
                                       "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
                                       "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
                                       "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
                                       "\\\\" "://"))
  (global-ligature-mode t))

(use-package hl-todo
  :hook (prog-mode . hl-todo-mode)
  :hook (yaml-mode . hl-todo-mode)
  :config
  (setq hl-todo-highlight-punctuation ":"
        hl-todo-keyword-faces
        '(("TODO" warning bold)
          ("FIXME" error bold)
          ("REVIEW" font-lock-keyword-face bold)
          ("HACK" font-lock-constant-face bold)
          ("DEPRECATED" font-lock-doc-face bold)
          ("NOTE" success bold)
          ("BUG" error bold)
          ("XXX" font-lock-constant-face bold))))

(use-package ido
  :hook (ido-mode . ido-ubiquitous-mode)
  :init
  (setq ido-save-directory-list-file (concat cache-dir "ido.last"))
  (ido-mode 1)
  :config
  (setq ido-ignore-buffers
        '("\\` " "^\\*ESS\\*" "^\\*Messages\\*" "^\\*[Hh]elp" "^\\*Buffer"
          "^\\*.*Completions\\*$" "^\\*Ediff" "^\\*tramp" "^\\*cvs-" "_region_"
          " output\\*$" "^TAGS$" "^\*Ido")
        ido-auto-merge-work-directories-length -1
        ido-confirm-unique-completion t
        ido-case-fold t
        ido-create-new-buffer 'always
        ido-enable-flex-matching t
        ido-everywhere t))

(use-package ido-vertical-mode
  :hook (ido-mode . ido-vertical-mode)
  :config (setq ido-vertical-show-count t))

(use-package ido-sort-mtime
  :hook (ido-mode . ido-sort-mtime-mode))

(use-package git-gutter
  :diminish
  :commands git-gutter:revert-hunk git-gutter:stage-hunk git-gutter:previous-hunk git-gutter:next-hunk
  :init
  (add-hook 'find-file-hook
            (defun +vc-gutter-init-maybe-h ()
              "Enable `git-gutter-mode' in the current buffer.
If the buffer doesn't represent an existing file, `git-gutter-mode's activation
is deferred until the file is saved. Respects `git-gutter:disabled-modes'."
              (let ((file-name (buffer-file-name (buffer-base-buffer))))
                (cond
                 ((and (file-remote-p (or file-name default-directory))
                       (not +vc-gutter-in-remote-files)))
                 ;; UX: If not a valid file, wait until it is written/saved to activate
                 ;;   git-gutter.
                 ((not (and file-name (vc-backend file-name)))
                  (add-hook 'after-save-hook #'+vc-gutter-init-maybe-h nil 'local))
                 ;; UX: Allow git-gutter or git-gutter-fringe to activate based on the
                 ;;   type of frame we're in. This allows git-gutter to work for silly
                 ;;   geese who open both tty and gui frames from the daemon.
                 ((if (and (display-graphic-p)
                           (require 'git-gutter-fringe nil t))
                      (setq-local git-gutter:init-function      #'git-gutter-fr:init
                                  git-gutter:view-diff-function #'git-gutter-fr:view-diff-infos
                                  git-gutter:clear-function     #'git-gutter-fr:clear
                                  git-gutter:window-width -1)
                    (setq-local git-gutter:init-function      'nil
                                git-gutter:view-diff-function #'git-gutter:view-diff-infos
                                git-gutter:clear-function     #'git-gutter:clear-diff-infos
                                git-gutter:window-width 1))
                  (unless (memq major-mode git-gutter:disabled-modes)
                    (git-gutter-mode +1)
                    (remove-hook 'after-save-hook #'+vc-gutter-init-maybe-h 'local)))))))

  ;; UX: Disable in Org mode, as per syl20bnr/spacemacs#10555 and
  ;;   syohex/emacs-git-gutter#24. Apparently, the mode-enabling function for
  ;;   global minor modes gets called for new buffers while they are still in
  ;;   `fundamental-mode', before a major mode has been assigned. I don't know
  ;;   why this is the case, but adding `fundamental-mode' here fixes the issue.
  (setq git-gutter:disabled-modes '(fundamental-mode image-mode pdf-view-mode))
  :config
  ;; PERF: Only enable the backends that are available, so it doesn't have to
  ;;   check when opening each buffer.
  (setq git-gutter:handled-backends
        (cons 'git (cl-remove-if-not #'executable-find (list 'hg 'svn 'bzr)
                                     :key #'symbol-name)))

  ;; UX: update git-gutter on focus (in case I was using git externally)
  (add-hook 'focus-in-hook #'git-gutter:update-all-windows)

  ;; Stop git-gutter doing things when we don't want
  (remove-hook 'post-command-hook #'git-gutter:post-command-hook)
  (advice-remove #'quit-window #'git-gutter:quit-window)
  (advice-remove #'switch-to-buffer #'git-gutter:switch-to-buffer)

  ;; UX: update git-gutter when using magit commands
  (advice-add #'magit-stage-file   :after #'+vc-gutter-update-h)
  (advice-add #'magit-unstage-file :after #'+vc-gutter-update-h)

  ;; UX: update git-gutter after reverting a buffer
  (add-hook 'after-revert-hook #'+vc-gutter-update-h))

(use-package multiple-cursors
  :config
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this))

(use-package exec-path-from-shell
  :init
  (exec-path-from-shell-initialize))

(use-package dumb-jump)

(use-package projectile
  :commands (projectile-project-root
             projectile-project-name
             projectile-project-p
             projectile-locate-dominating-file
             projectile-relevant-known-projects)
  :init
  (setq projectile-cache-file (concat cache-dir "projectile.cache")
        ;; Auto-discovery is slow to do by default. Better to update the list
        ;; when you need to (`projectile-discover-projects-in-search-path').
        projectile-auto-discover nil
        projectile-enable-caching (not noninteractive)
        projectile-globally-ignored-files '(".DS_Store" "TAGS")
        projectile-globally-ignored-file-suffixes '(".elc" ".pyc" ".o")
        projectile-kill-buffers-filter 'kill-only-files
        projectile-known-projects-file (concat cache-dir "projectile.projects")
        projectile-ignored-projects '("~/"))

  (global-set-key [remap evil-jump-to-tag] #'projectile-find-tag)
  (global-set-key [remap find-tag]         #'projectile-find-tag))

(use-package amx
  :init
  (amx-mode 1))

(use-package solaire-mode
  :hook (prog-mode-hook . solaire-global-mode)
  :hook (+popup-buffer-mode . turn-on-solaire-mode))

(use-package hl-line
  ;; Highlights the current line
  :hook (dashboard-setup-startup-hook . global-hl-line-mode)
  :init
  (defvar global-hl-line-modes
    '(prog-mode text-mode conf-mode special-mode
                org-agenda-mode dired-mode)
    "What modes to enable `hl-line-mode' in.")
  :config
  ;; HACK I reimplement `global-hl-line-mode' so we can white/blacklist modes in
  ;;      `global-hl-line-modes' _and_ so we can use `global-hl-line-mode',
  ;;      which users expect to control hl-line in Emacs.
  (define-globalized-minor-mode global-hl-line-mode hl-line-mode
    (lambda ()
      (and (cond (hl-line-mode nil)
                 ((null global-hl-line-modes) nil)
                 ((eq global-hl-line-modes t))
                 ((eq (car global-hl-line-modes) 'not)
                  (not (derived-mode-p global-hl-line-modes)))
                 ((apply #'derived-mode-p global-hl-line-modes)))
           (hl-line-mode +1)))))

(use-package winner
  ;; undo/redo changes to Emacs' window layout
  :preface (defvar winner-dont-bind-my-keys t) ; I'll bind keys myself
  :hook (dashboard-setup-startup-hook . winner-mode)
  :config
  (appendq! winner-boring-buffers
            '("*Compile-Log*" "*inferior-lisp*" "*Fuzzy Completions*"
              "*Apropos*" "*Help*" "*cvs*" "*Buffer List*" "*Ibuffer*"
              "*esh command on file*")))

(use-package paren
  ;; highlight matching delimiters
  :hook (dashboard-setup-startup-hook . show-paren-mode)
  :config
  (setq show-paren-delay 0.1
        show-paren-highlight-openparen t
        show-paren-when-point-inside-paren t
        show-paren-when-point-in-periphery t))

(use-package nerd-icons
  :commands (nerd-icons-octicon
             nerd-icons-faicon
             nerd-icons-flicon
             nerd-icons-wicon
             nerd-icons-mdicon
             nerd-icons-codicon
             nerd-icons-devicon
             nerd-icons-ipsicon
             nerd-icons-pomicon
             nerd-icons-powerline))

;; Many major modes do no highlighting of number literals, so we do it for them
(use-package highlight-numbers
  :hook ((prog-mode conf-mode) . highlight-numbers-mode)
  :config (setq highlight-numbers-generic-regexp "\\_<[[:digit:]]+\\(?:\\.[0-9]*\\)?\\_>"))

(use-package rainbow-delimiters
  :init
  (rainbow-delimiters-mode t)
  :config
  (setq rainbow-delimiters-max-face-count 4))

(use-package undo-fu
  :config
  ;; Increase undo history limits to reduce likelihood of data loss
  (setq undo-limit 400000           ; 400kb (default is 160kb)
        undo-strong-limit 3000000   ; 3mb   (default is 240kb)
        undo-outer-limit 48000000)  ; 48mb  (default is 24mb)
  (define-minor-mode undo-fu-mode
    "Enables `undo-fu' for the current session."
    :keymap (let ((map (make-sparse-keymap)))
              (define-key map [remap undo] #'undo-fu-only-undo)
              (define-key map [remap redo] #'undo-fu-only-redo)
              (define-key map (kbd "C-_")     #'undo-fu-only-undo)
              (define-key map (kbd "M-_")     #'undo-fu-only-redo)
              (define-key map (kbd "C-M-_")   #'undo-fu-only-redo-all)
              (define-key map (kbd "C-x r u") #'undo-fu-session-save)
              (define-key map (kbd "C-x r U") #'undo-fu-session-recover)
              map)
    :init-value nil
    :global t)
  (undo-fu-mode))

(use-package undo-fu-session
  :hook (undo-fu-mode . global-undo-fu-session-mode)
  :custom (undo-fu-session-directory (concat user-emacs-directory "undo-fu-session/"))
  :config
  (setq undo-fu-session-incompatible-files '("\\.gpg$" "/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'"))
  (when (executable-find "zstd")
    ;; There are other algorithms available, but zstd is the fastest, and speed
    ;; is our priority within Emacs
    (setq undo-fu-session-compression 'zst))
  (defadvice! +undo-fu-make-hashed-session-file-name-a (file)
              :override #'undo-fu-session--make-file-name
              (concat (let ((backup-directory-alist `(("." . ,undo-fu-session-directory))))
                        (make-backup-file-name-1 file))
                      (undo-fu-session--file-name-ext))))

(use-package undo-tree
  :hook (undo-fu-mode . global-undo-tree-mode)
  :custom (undo-tree-history-directory-alist `(("." . ,(concat user-emacs-directory "undo-tree-hist/"))))
  :config
  (setq undo-tree-visualizer-diff t
        undo-tree-auto-save-history t
        undo-tree-enable-undo-in-region t
        ;; Increase undo limits to avoid emacs prematurely truncating the undo
        ;; history and corrupting the tree. This is larger than the undo-fu
        ;; defaults because undo-tree trees consume exponentially more space,
        ;; and then some when `undo-tree-enable-undo-in-region' is involved. See
        ;; syl20bnr/spacemacs#12110
        undo-limit 800000            ; 800kb (default is 160kb)
        undo-strong-limit 12000000   ; 12mb  (default is 240kb)
        undo-outer-limit 128000000)) ; 128mb (default is 24mb))

(use-package ibuffer-projectile
  ;; Group ibuffer's list by project root
  :hook (ibuffer . ibuffer-projectile-set-filter-groups)
  :config
  (setq ibuffer-projectile-prefix
        (if (modulep! +icons)
            (concat (nerd-icons-octicon
                     "nf-oct-file_directory"
                     :face ibuffer-filter-group-name-face
                     :v-adjust -0.05)
                    " ")
          "Project: ")))

;; Yes, I really want to quit.
(setq confirm-kill-emacs nil)
