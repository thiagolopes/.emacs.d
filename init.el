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
  (setq dashboard-set-navigator t
        dashboard-projects-backend 'projectile
        dashboard-icon-type 'all-the-icons
        dashboard-show-shortcuts t
        dashboard-items '((recents  . 8)
                          (bookmarks . 5)
                          (projects . 5)
                          (agenda . 2)
                          (registers . 2))
        dashboard-center-content t)
  (dashboard-setup-startup-hook))

(use-package cider
  :config
  (setq nrepl-hide-special-buffers t
        nrepl-log-messages nil
        cider-font-lock-dynamically '(macro core function var deprecated)
        cider-overlays-use-font-lock t
        cider-prompt-for-symbol nil
        cider-repl-history-display-duplicates nil
        cider-repl-history-display-style 'one-line
        cider-repl-history-file (concat user-emacs-directory "cache/cider-repl-history")
        cider-repl-history-highlight-current-entry t
        cider-repl-history-quit-action 'delete-and-restore
        cider-repl-history-highlight-inserted-item t
        cider-repl-history-size 1000
        cider-repl-result-prefix ";; => "
        cider-repl-print-length 100
        cider-repl-use-clojure-font-lock t
        cider-repl-use-pretty-printing t
        cider-repl-wrap-history nil
        cider-stacktrace-default-filters '(tooling dup)
        cider-repl-display-help-banner nil
        cider-repl-pop-to-buffer-on-connect 'display-only))

(use-package company
  :diminish
  :config
  (global-company-mode t)
  :config
  (setq company-idle-delay 0.0
        company-minimum-prefix-length 1))

(use-package hl-todo
  :hook (prog-mode-hook . hl-todo-mode)
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

(use-package git-gutter-fringe
  :after (git-gutter)
  :config
  (require 'git-gutter-fringe))

(use-package multiple-cursors
  :config
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this))

(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

(use-package projectile
  :commands (projectile-project-root
             projectile-project-name
             projectile-project-p
             projectile-locate-dominating-file
             projectile-relevant-known-projects)
  :init
  (setq projectile-project-search-path '("~/dev/" "~/work/" "~/.emacs.d/" ("~/github" . 1)))
  (setq projectile-cache-file (concat cache-dir "cache/projectile/")
        ;; Auto-discovery is slow to do by default. Better to update the list
        ;; when you need to (`projectile-discover-projects-in-search-path').
        projectile-auto-discover nil
        projectile-enable-caching (not noninteractive)
        projectile-globally-ignored-files '(".DS_Store" "TAGS")
        projectile-globally-ignored-file-suffixes '(".elc" ".pyc" ".o")
        projectile-kill-buffers-filter 'kill-only-files
        projectile-known-projects-file (concat projectile-cache-file "projectile.projects")
        projectile-ignored-projects '("~/"))

  (global-set-key [remap evil-jump-to-tag] #'projectile-find-tag)
  (global-set-key [remap find-tag]         #'projectile-find-tag))

(use-package amx
  :init
  (setq amx-save-file (concat user-emacs-directory "cache/amx-items")))

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
  (winner-mode 1)
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
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
  (setq rainbow-delimiters-max-face-count 4))

(use-package undo-tree
  :init
  (global-undo-tree-mode)
  (add-hook 'prog-mode-hook #'undo-tree-mode)
  :config
  (setq undo-tree-visualizer-diff t
        undo-tree-auto-save-history t
        undo-tree-history-directory-alist `(("." . ,(concat user-emacs-directory "cache/fu/undo-tree-hist/")))
        undo-tree-enable-undo-in-region t
        ;; Increase undo limits to avoid emacs prematurely truncating the undo
        ;; history and corrupting the tree. This is larger than the undo-fu
        ;; defaults because undo-tree trees consume exponentially more space,
        ;; and then some when `undo-tree-enable-undo-in-region' is involved. See
        ;; syl20bnr/spacemacs#12110
        undo-limit 800000            ; 800kb (default is 160kb)
        undo-strong-limit 12000000   ; 12mb  (default is 240kb)
        undo-outer-limit 128000000)) ; 128mb (default is 24mb))

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
  :custom
  (undo-fu-session-directory (concat user-emacs-directory "cache/fu/undo-fu-session/"))
  :config
  (undo-fu-session-global-mode t)
  (setq undo-fu-session-incompatible-files '("\\.gpg$" "/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'"))
  (when (executable-find "zstd")
    ;; There are other algorithms available, but zstd is the fastest, and speed
    ;; is our priority within Emacs
    (setq undo-fu-session-compression 'zst)))

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

(use-package magit
  :init
  (setq magit-auto-revert-mode nil)  ; we do this ourselves further down
  ;; Must be set early to prevent ~/.config/emacs/transient from being created
  (let ((data-dir user-emacs-directory))
    (setq transient-levels-file  (concat data-dir "transient/levels")
          transient-values-file  (concat data-dir "transient/values")
          transient-history-file (concat data-dir "transient/history")))
  :config
  (setq transient-default-level 5
        magit-diff-refine-hunk t ; show granular diffs in selected hunk
        ;; Don't autosave repo buffers. This is too magical, and saving can
        ;; trigger a bunch of unwanted side-effects, like save hooks and
        ;; formatters. Trust the user to know what they're doing.
        magit-save-repository-buffers nil
        ;; Don't display parent/related refs in commit buffers; they are rarely
        ;; helpful and only add to runtime costs.
        magit-revision-insert-related-refs nil)
  (add-hook 'magit-process-mode-hook #'goto-address-mode))

(use-package drag-stuff
  :init
  (general-define-key "<M-up>"    #'drag-stuff-up
                      "<M-down>"  #'drag-stuff-down
                      "<M-left>"  #'drag-stuff-left
                      "<M-right>" #'drag-stuff-right))


(use-package ivy
  :diminish
  :init
  (ivy-mode)
  ;; Fix #4886: otherwise our remaps are overwritten
  (setq ivy-mode-map (make-sparse-keymap))
  :config
  ;; The default sorter is much to slow and the default for `ivy-sort-max-size'
  ;; is way too big (30,000). Turn it down so big repos affect project
  ;; navigation less.
  (setq ivy-sort-max-size 7500)
  ;; Counsel changes a lot of ivy's state at startup; to control for that, we
  ;; need to load it as early as possible. Some packages (like `ivy-prescient')
  ;; require this.
  (require 'counsel nil t)
  (setq ivy-height 17
        ivy-wrap t
        ivy-count-format "(%d/%d) "
        ivy-fixed-height-minibuffer t
        ivy-read-action-function #'ivy-hydra-read-action
        ivy-read-action-format-function #'ivy-read-action-format-columns
        ;; don't show recent files in switch-buffer
        ivy-use-virtual-buffers nil
        ;; ...but if that ever changes, show their full path
        ivy-virtual-abbreviate 'abbreviate
        ;; don't quit minibuffer on delete-error
        ivy-on-del-error-function #'ignore
        ;; enable ability to select prompt (alternative to `ivy-immediate-done')
        ivy-use-selectable-prompt t))

(use-package counsel
  :init
  (general-define-key
   [remap apropos]                  #'counsel-apropos
   [remap bookmark-jump]            #'counsel-bookmark
   [remap compile]                  #'counsel-compile
   [remap describe-bindings]        #'counsel-descbinds
   [remap describe-face]            #'counsel-faces
   [remap describe-function]        #'counsel-describe-function
   [remap describe-variable]        #'counsel-describe-variable
   [remap describe-symbol]          #'counsel-describe-symbol
   [remap evil-show-registers]      #'counsel-evil-registers
   [remap evil-show-marks]          #'counsel-mark-ring
   [remap execute-extended-command] #'counsel-M-x
   [remap find-file]                #'counsel-find-file
   [remap find-library]             #'counsel-find-library
   [remap imenu]                    #'counsel-imenu
   [remap info-lookup-symbol]       #'counsel-info-lookup-symbol
   [remap load-theme]               #'counsel-load-theme
   [remap locate]                   #'counsel-locate
   [remap org-goto]                 #'counsel-org-goto
   [remap org-set-tags-command]     #'counsel-org-tag
   [remap projectile-compile-project] #'projectile-compile
   [remap recentf-open-files]       #'counsel-recentf
   [remap set-variable]             #'counsel-set-variable
   [remap swiper]                   #'counsel-grep-or-swiper
   [remap insert-char]              #'counsel-unicode-char
   [remap yank-pop]                 #'counsel-yank-pop)
  :config
  ;; Don't use ^ as initial input. Set this here because `counsel' defines more
  ;; of its own, on top of the defaults.
  (setq ivy-initial-inputs-alist nil)
  ;; Integrate with `helpful'
  (setq counsel-describe-function-function #'helpful-callable
        counsel-describe-variable-function #'helpful-variable
        counsel-descbinds-function #'helpful-callable)

  ;; Record in jumplist when opening files via counsel-{ag,rg,pt,git-grep}
  (add-hook 'counsel-grep-post-action-hook #'better-jumper-set-jump)

  (add-to-list 'counsel-compile-root-functions #'projectile-project-root)
  (add-to-list 'ivy-sort-functions-alist '(counsel-imenu))

  ;; `counsel-find-file'
  (setq counsel-find-file-ignore-regexp "\\(?:^[#.]\\)\\|\\(?:[#~]$\\)\\|\\(?:^Icon?\\)")
  (dolist (fn '(counsel-rg counsel-find-file))
    (ivy-add-actions
     fn '(("p" (lambda (path) (with-ivy-window (insert (file-relative-name path default-directory))))
           "insert relative path")
          ("P" (lambda (path) (with-ivy-window (insert path)))
           "insert absolute path")
          ("l" (lambda (path) (with-ivy-window (insert (format "[[./%s]]" (file-relative-name path default-directory)))))
           "insert relative org-link")
          ("L" (lambda (path) (with-ivy-window (insert (format "[[%s]]" path))))
           "Insert absolute org-link"))))
  (setq swiper-action-recenter t)
  (ivy-add-actions 'counsel-file-jump (plist-get ivy--actions-list 'counsel-find-file)))

(use-package ivy-prescient
  :hook (ivy-mode . ivy-prescient-mode)
  :hook (ivy-prescient-mode . prescient-persist-mode)
  :config
  ;; REVIEW Remove when radian-software/prescient.el#102 is resolved
  (add-to-list 'ivy-sort-functions-alist '(ivy-resume))
  (setq ivy-prescient-sort-commands
        '(:not swiper swiper-isearch ivy-switch-buffer lsp-ivy-workspace-symbol
               ivy-resume ivy--restore-session counsel-grep counsel-git-grep
               counsel-rg counsel-ag counsel-ack counsel-fzf counsel-pt counsel-imenu
               counsel-yank-pop counsel-recentf counsel-buffer-or-recentf
               counsel-outline counsel-org-goto counsel-jq)
        ivy-prescient-retain-classic-highlighting t)
  (defun +ivy-prescient-non-fuzzy (str)
    (let ((prescient-filter-method '(literal regexp)))
      (ivy-prescient-re-builder str)))

  ;; NOTE prescient config duplicated with `company'
  (setq prescient-save-file (concat user-emacs-directory "cache/prescient-save.el")))

(use-package lsp-mode
  :commands lsp-install-server
  :init
  ;; Don't touch ~/.emacs.d, which could be purged without warning
  (setq lsp-warn-no-matched-clients nil)
  (add-hook 'prog-mode-hook #'lsp)
  (setq lsp-auto-guess-root t)
  (setq lsp-session-file (concat user-emacs-directory "cache/lsp-session")
        lsp-server-install-dir (concat user-emacs-directory "cache/lsp"))
  ;; Don't auto-kill LSP server after last workspace buffer is killed, because I
  ;; will do it for you, after `+lsp-defer-shutdown' seconds.
  (setq lsp-keep-workspace-alive nil)

  ;; Disable features that have great potential to be slow.
  (setq lsp-enable-folding nil
        lsp-enable-text-document-color nil)
  ;; Reduce unexpected modifications to code
  (setq lsp-enable-on-type-formatting nil)
  ;; Make breadcrumbs opt-in; they're redundant with the modeline and imenu
  (setq lsp-headerline-breadcrumb-enable nil))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (setq lsp-ui-peek-enable t
        lsp-ui-doc-max-height 8
        lsp-ui-doc-max-width 72         ; 150 (default) is too wide
        lsp-ui-doc-delay 0.75           ; 0.2 (default) is too naggy
        lsp-ui-doc-show-with-mouse nil  ; don't disappear on mouseover
        lsp-ui-doc-position 'at-point
        lsp-ui-sideline-ignore-duplicate t
        lsp-ui-doc-show-with-mouse t
        ;; Don't show symbol definitions in the sideline. They are pretty noisy,
        ;; and there is a bug preventing Flycheck errors from being shown (the
        ;; errors flash briefly and then disappear).
        lsp-ui-sideline-show-hover nil))

(use-package lsp-ivy
  :commands lsp-ivy-workspace-symbol lsp-ivy-global-workspace-symbol)

(use-package consult-lsp
  :after (lsp)
  :config
  (define-key lsp-mode-map [remap xref-find-apropos] #'consult-lsp-symbols))

(use-package dumb-jump
  :commands dumb-jump-result-follow
  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  (setq dumb-jump-default-project (concat user-emacs-directory "cache/jump")
        dumb-jump-prefer-searcher 'ag
        dumb-jump-aggressive nil
        dumb-jump-selector 'ivy)
  (add-hook 'dumb-jump-after-jump-hook #'better-jumper-set-jump))

(use-package whitespace-cleanup-mode
  :diminish
  :config
  (add-hook 'prog-mode-hook 'whitespace-cleanup-mode))

(use-package git-timemachine)

(use-package which-key
  :diminish
  :init
  (which-key-mode t))

(use-package smart-compile
  :init
  (general-define-key "<f8>" #'smart-compile))

(use-package marginalia
  :init
  (marginalia-mode t))

(use-package mode-line-bell
  :init
  (mode-line-bell-mode t))

(use-package goto-last-change
  :config
  (general-define-key "C-;" #'goto-last-change))

(use-package popwin
  :config
  (popwin-mode t))

(use-package no-littering
  :config
  (setq auto-save-file-name-transforms
        `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))))

(use-package nvm
  :config
  (when (file-exists-p "~/.nvmrc")
    (nvm-use-for)))

(use-package helpful
  :config
  (general-define-key
   "C-h f"   #'helpful-callable
   "C-h v"   #'helpful-variable
   "C-h k"   #'helpful-key
   "C-h x"   #'helpful-command
   "C-c C-d" #'helpful-at-point
   "C-h F"   #'helpful-function)
  (setq counsel-describe-function-function #'helpful-callable
        counsel-describe-variable-function #'helpful-variable))

(use-package solaire-mode
  :config
  ;; Made dashboard a real buffer
  (setq solaire-mode-real-buffer-fn #'(lambda () (or (solaire-mode-real-buffer-p)
                                                (equal (buffer-name) "*dashboard*"))))
  (solaire-global-mode t))

(use-package buffer-name-relative
  :config
  (buffer-name-relative-mode))

(use-package smart-mode-line
  :config
  (sml/setup))
