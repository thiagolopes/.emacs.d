;;; init.el --- Main file      -*- lexical-binding: t; -*-
;;; Commentary:
;;; basic Emacs configuration, by thiagolopes
;;; Code:


(message "[config] start init.el")
(add-hook 'after-init-hook (lambda () (message "[config] finish init.el")))

(when (version< emacs-version "31.1")
  (message (format "Min version is < %s. Please upgrade." emacs-version)))

;; identify the system
(setq-local *is-a-mac* (eq system-type 'darwin))
(setq-local *is-a-linux* (or (eq system-type 'gnu/linux) (eq system-type 'linux)))

(setq-default
 user-full-name "Thiago Lopes"
 user-mail-address "thiagolopes@protonmail.com")

;;; UTILITES

(when *is-a-mac*
  ;; do not resize on scroll
  ;; https://xenodium.com/hey-mouse-dont-mess-with-my-emacs-font-size
  (global-set-key (kbd "<pinch>") 'ignore)
  (global-set-key (kbd "<C-wheel-up>") 'ignore)
  (global-set-key (kbd "<C-wheel-down>") 'ignore)

  (setq-default mac-control-modifier 'control)
  (setq-default mac-command-modifier 'meta)
  (setq-default mac-right-option-modifier 'control))

(defun kill-all-buffers ()
  "Kill all open buffers."
  (interactive)
  (dolist (buf (buffer-list))
    (unless (string= (buffer-name buf) "*scratch*")
      (kill-buffer buf))))

(defun bool-to-int (bool)
  "BOOL -> INT."
  (if bool 1 0))

;;; PACKAGES
(require 'package)
;; TODO deal to worker without internet for some reason, emergencial mode.
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; Do not change package-selected-packages (custom.el) when package-install
(advice-add 'package--save-selected-packages :override #'ignore)

(setq-default
 package-selected-packages
 '(
   ace-jump-mode
   anzu
   beacon
   cape
   consult
   diff-hl
   diminish
   dumb-jump
   eglot
   elfeed
   embark
   embark-consult
   exec-path-from-shell
   expand-region
   flycheck
   git-link
   git-timemachine
   goto-last-change
   goto-line-preview
   helpful
   hl-todo
   hungry-delete
   magit
   move-dup
   multiple-cursors
   mwim
   no-littering
   nvm
   orderless
   org-appear
   org-download
   org-roam
   page-break-lines
   pdf-tools
   popwin
   prism
   rainbow-delimiters
   rainbow-mode
   rg
   sudo-edit
   undo-fu
   undo-fu-session
   uuidgen
   verb
   volatile-highlights
   vundo
   yafolding
   zzz-to-char

   ;; Themes
   amaranth-dark-theme
   ef-themes
   gruber-darker-theme
   modus-themes
   purp-theme
   srcery-theme
   standard-themes
   zenburn-theme

   ;; File Modes
   cmake-mode
   deno-ts-mode
   dockerfile-mode
   fish-mode
   go-mode
   hledger-mode
   i3wm-config-mode
   markdown-mode
   meson-mode
   rust-mode
   svelte-mode
   typescript-mode
   web-mode
   yaml-mode
   zig-mode
   ))

(eval-and-compile
  (progn
    ;; 3th lib 'no-littering' is required
    (when (not (package-installed-p 'no-littering))
      (package-install 'no-littering))
    (require 'no-littering)

    ;; dash is also required
    (when (not (package-installed-p 'dash))
      (package-install 'dash))
    (require 'dash)

    (defun list-missing-pckgs ()
      "List all missing packages."
      (-filter
       (-not #'package-installed-p)
       package-selected-packages))

    ;; install packages
    (let ((packages-not-installed (list-missing-pckgs))
          (should-restart? t))
      (when packages-not-installed
        (-each packages-not-installed #'package-install)
        (when should-restart?
          ;; OVERKILL REACTION
          (restart-emacs))))))

(require 'ansi-color)

(defmacro no-litterize (path var)
  "Set VAR to no-littering as PATH."
  `(let ((dir (no-littering-expand-var-file-name ,path)))
     (make-directory dir t)
     (setq ,var `((".*" ,dir t)))))
(no-litterize "lock-files/" lock-file-name-transforms)
(no-litterize "backups/" backup-directory-alist)

(global-set-key (kbd "C-x C-d")  'dired)
(global-set-key (kbd "C-x C-b")  'ibuffer)
(global-set-key (kbd "M-!")      'async-shell-command)
(global-set-key (kbd "C-,")      'duplicate-line)

(global-set-key (kbd "S-C-<right>")  'shrink-window-horizontally)
(global-set-key (kbd "S-C-<left>")   'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<up>")     'shrink-window)
(global-set-key (kbd "S-C-<down>")   'enlarge-window)

(global-set-key [remap goto-line]          'goto-line-preview)
(global-set-key [remap goto-line-relative] 'goto-line-preview-relative)
(global-set-key (kbd "C-x /")              'goto-last-change)

(global-set-key (kbd "C-.") 'embark-act)

(global-set-key (kbd "M-y"    ) 'consult-yank-pop)
(global-set-key (kbd "C-x b"  ) 'consult-buffer)
(global-set-key (kbd "C-c p"  ) 'consult-project-buffer)
(global-set-key (kbd "C-c m"  ) 'consult-global-mark)
(global-set-key (kbd "C-c f"  ) 'consult-find)
(global-set-key (kbd "C-c i"  ) 'consult-imenu)
(global-set-key (kbd "C-c r"  ) 'consult-ripgrep)
(global-set-key (kbd "C-c l"  ) 'consult-line-multi)
(global-set-key (kbd "M-g g"  ) 'consult-goto-line)
(global-set-key (kbd "M-g M-g") 'consult-goto-line)

(global-set-key (kbd "C-c j") 'ace-jump-mode)

(global-set-key (kbd "M-@") 'er/expand-region)

(global-set-key (kbd "C->")     'mc/mark-next-like-this)
(global-set-key (kbd "C-<")     'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/edit-lines)
(global-set-key (kbd "C-c C->") 'mc/mark-all-like-this)

(global-set-key (kbd "C-a") 'mwim-beginning)
(global-set-key (kbd "C-e") 'mwim-end)

(global-set-key (kbd "M-u") 'upcase-dwim)
(global-set-key (kbd "M-l") 'downcase-dwim)
(global-set-key (kbd "M-c") 'capitalize-dwim)

(global-set-key (kbd "M-<up>"  ) 'move-dup-move-lines-up)
(global-set-key (kbd "M-<down>") 'move-dup-move-lines-down)

(global-set-key (kbd "M-z")   #'zzz-to-char)
(global-set-key (kbd "<f9>")  (lambda () (interactive)
                                "Run compile on project if available."
                                (if (project-current) (project-compile)
                                  (call-interactively #'compile))))

(defun flycheck-configure ()
  "Configure flycheck."
  (global-flycheck-mode 1)
  (global-flycheck-eglot-mode 1)
  ;; (setq-default flycheck-indication-mode 'left-margin) ;; this use margin instead fringe
  (setq-default
   flycheck-annotate-mode 'oel
   flymake-start-on-save-buffer nil
   flymake-start-on-flymake-mode nil
   flycheck-highlighting-style '(delimiters "" "")
   flycheck-mode-line-prefix " "
   flycheck-eglot-exclusive nil))

(defun org-mode-configure ()
  "Configure Org."
  (add-hook 'org-mode-hook #'org-appear-mode)
  (add-hook 'org-mode-hook #'visual-line-mode)
  (setq-default org-return-follows-link t
          org-startup-folded 'fold
          org-hide-emphasis-markers t)
  ;; TODO use envvar
  (when-let* ((custom-org-roam-envvar (exec-path-from-shell-getenv "ORG_ROAM_DIR"))
              (custom-org-roam (file-truename custom-org-roam-envvar))
              (exist-p (file-directory-p custom-org-roam)))
    (setq-default org-roam-directory custom-org-roam)))

(defun dump-jump-configure ()
  "Configure Dump Jump."
  ;; enable dump-jump as a option in xref-backend-functions
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  (setq-default dumb-jump-prefer-searcher 'rg)
  ;; consult required
  (setq-default xref-show-definitions-function #'consult-xref))

(defun diff-hl-configure ()
  "Configure lateral diff on buffers."
  (add-hook 'prog-mode-hook #'diff-hl-mode)
  (add-hook 'prog-mode-hook #'diff-hl-margin-mode)
  (add-hook 'dired-mode-hook #'diff-hl-dired-mode)
  (setq-default
   diff-hl-disable-on-remote t
   diff-hl-fringe-bmp-function 'diff-hl-fringe-bmp-from-type)
  (with-eval-after-load 'diff-hl
    (custom-set-faces
     `(diff-hl-change ((t (:foreground ,(face-foreground 'diff-changed nil 'default) :background nil))))
     `(diff-hl-delete ((t (:foreground ,(face-foreground 'diff-removed nil 'default) :background nil))))
     `(diff-hl-insert ((t (:foreground ,(face-foreground 'diff-added nil 'default) :background nil)))))))

(defun helpful-configure ()
  "Switch all describe-* to use helpful."
  (advice-add 'describe-function :override #'helpful-function)
  (advice-add 'describe-command  :override #'helpful-command)
  (advice-add 'describe-key      :override #'helpful-key)
  (advice-add 'describe-variable :override #'helpful-variable)
  (advice-add 'describe-symbol   :override #'helpful-symbol))

(defun whitespace-configure ()
  "Configure whitespace."
  (setq-default whitespace-style '(face spaces empty tabs newline trailing space-mark tab-mark))
  (add-hook 'before-save-hook #'whitespace-cleanup))

(defun popwin-configure ()
  "Configure popwin - avoid bottom buffers to close on C-g or loose focus."
  (popwin-mode t)
  (push "*eldoc*" popwin:special-display-config)
  (push "*Warnings*" popwin:special-display-config)
  (push "*Flycheck errors*" popwin:special-display-config))

(defun exec-path-from-shell-configure ()
  (setq-default
   exec-path-from-shell-shell-name "/bin/zsh"
   exec-path-from-shell-variables '("PATH" "ORG_ROAM_DIR" "ELFEED_FILE"))
  (exec-path-from-shell-initialize))

(defun elfeed-configure ()
  (when-let* ((elfeed-envvar (exec-path-from-shell-getenv "ELFEED_FILE"))
              (elfeed-file (file-truename elfeed-envvar))
              (exists! (file-exists-p elfeed-file)))
    (setq-local elfeed-file-el (file-truename "~/doc/rss.el"))
    (when (file-exists-p elfeed-file-el) (load-file elfeed-file-el))))


(windmove-default-keybindings) ;; moviment with shift+arrow

(exec-path-from-shell-configure)
(diff-hl-configure)
(dump-jump-configure)
(flycheck-configure)
(helpful-configure)
(org-mode-configure)
(popwin-configure)
(whitespace-configure)
(elfeed-configure)


(add-hook 'emacs-startup-hook
          (lambda ()
            (progn
              (split-window-right)
              (switch-to-buffer "*scratch*"))))

(add-hook 'prog-mode-hook 'hl-todo-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

(add-hook 'text-mode-hook 'flyspell-mode)

(add-hook 'hledger-mode-hook (lambda () (whitespace-mode)))

(add-hook 'compilation-filter-hook 'ansi-color-compilation-filter)

;; (add-hook 'completion-at-point-functions #'cape-keyword)
;; (add-hook 'completion-at-point-functions #'cape-abbrev)
;; (add-hook 'completion-at-point-functions #'cape-dabbrev)
;; (add-hook 'completion-at-point-functions #'cape-file)
;; (add-hook 'completion-at-point-functions #'cape-elisp-block)
;; (add-hook 'completion-at-point-functions #'cape-dict)
;; (add-hook 'completion-at-point-functions #'cape-history)


(setq-default
 eglot-autoshutdown t
 eglot-extend-to-xref t
 eglot-ignored-server-capabilities '(:codeLensProvider
				     :hoverProvider
				     :documentOnTypeFormattingProvider
				     :foldingRangeProvider :inlayHintProvider))

(setq-default
 font-use-system-font t
 frame-resize-pixelwise t
 window-resize-pixelwise t
 mode-line-compact 'long
 prefer-coding-system 'utf-8)

(setopt
 save-interprogram-paste-before-kill t
 mouse-yank-at-point t
 mouse-drag-and-drop-region t
 mouse-drag-and-drop-region-cross-program t
 mouse-drag-mode-line-buffer t)

(setq-default
 imenu-auto-rescan t
 view-read-only t)

(setopt
 vc-deduce-backend-nonvc-modes t
 vc-dir-save-some-buffers-on-revert t
 vc-find-revision-no-save t
 vc-follow-symlinks t
 vc-use-incoming-outgoing-prefixes t)

(setopt
 minibuffer-visible-completions t
 completions-detailed t
 completions-group t
 completions-max-height 20
 completion-auto-select 'second-tab
 completion-eager-update t
 global-completion-preview-mode t
 tab-always-indent 'complete
 package-menu-use-current-if-no-marks nil
 frame-inhibit-implied-resize t
 compilation-scroll-output 'first-error
 compilation-always-kill t)

(setq-default
 zenburn-override-colors-alist
 '(("zenburn-fg+1" . "#FFFFEF") ("zenburn-fg" . "#DCDCCC") ("zenburn-fg-1" . "#70705E") ("zenburn-bg-2" . "#000000")
   ("zenburn-bg-1" . "#202020") ("zenburn-bg-05" . "#2D2D2D") ("zenburn-bg" . "#313131") ("zenburn-bg+05" . "#383838")
   ("zenburn-bg+1" . "#3E3E3E") ("zenburn-bg+2" . "#4E4E4E") ("zenburn-bg+3" . "#5E5E5E") ("zenburn-red+1" . "#E9B0B0")
   ("zenburn-red" . "#D9A0A0") ("zenburn-red-1" . "#C99090") ("zenburn-red-2" . "#B98080") ("zenburn-red-3" . "#A97070")
   ("zenburn-red-4" . "#996060") ("zenburn-orange" . "#ECBC9C") ("zenburn-yellow" . "#FDECBC") ("zenburn-yellow-1" . "#EDDCAC")
   ("zenburn-yellow-2" . "#DDCC9C") ("zenburn-green-1" . "#6C8C6C") ("zenburn-green" . "#8CAC8C") ("zenburn-green+1" . "#9CBF9C")
   ("zenburn-green+2" . "#ACD2AC") ("zenburn-green+3" . "#BCE5BC") ("zenburn-green+4" . "#CCF8CC") ("zenburn-cyan" . "#A0EDF0")
   ("zenburn-blue+1" . "#9CC7FB") ("zenburn-blue" . "#99DDE0") ("zenburn-blue-1" . "#89C5C8") ("zenburn-blue-2" . "#79ADB0")
   ("zenburn-blue-3" . "#699598") ("zenburn-blue-4" . "#597D80") ("zenburn-blue-5" . "#436D6D") ("zenburn-magenta" . "#E090C7")))

(setq-default
 ef-dream-palette-overrides
 '((bg-main               "#131015")
   (bg-hl-line            "#232224")
   (fg-mode-line-active   "#f2ddcf")
   (bg-mode-line-active   "#472b00")
   (yellow-cooler         "#ff9f0a")
   (bg-hl-line            "#2e1a3a")
   (bg-hl-line            "#352102")
   (bg-hl-line            "#3b393e")
   (bg-mode-line-inactive "#5E4527")))

(setopt
 treesit-enabled-modes t
 treesit-auto-install-grammar 'always)

(setq-default display-line-numbers-type 'relative) ;; TODO implement linum relative in goto-line-preview-relative

(setq-default
 dired-kill-when-opening-new-dired-buffer t
 dired-listing-switches "-alh"
 dired-auto-revert-buffer t
 dired-mouse-drag-files t)

(setq-default
 c-basic-offset 4
 c-ts-mode-indent-offset 4)

(setopt
 ;; completion-auto-help nil
 ;; custom-buffer-sort-alphabetically t
 ;; display-line-numbers-width 4
 ;; ibuffer-expert t
 ;; indicate-buffer-boundaries 'left
 ;; inhibit-startup-screen t
 ;; read-buffer-completion-ignore-case t
 ;; read-file-name-completion-ignore-case t
 ;; scroll-conservatively 1000
 ;; scroll-margin 1
 ;; scroll-preserve-screen-position t
 ;; treesit-enabled-modes t
 ;; use-dialog-box nil

 ;; delete-selection-mode          t
 ace-jump-mode-case-fold        t
 backup-by-copying              t
 beacon-mode                    t
 beacon-size                    80
 blink-cursor-mode              t
 column-number-mode             t
 context-menu-mode              t ;; enable menu with mouse right click
 cursor-type                    'box
 custom-buffer-indent           4
 custom-safe-themes             t
 editorconfig-mode              t
 electric-pair-mode             nil ;; auto close pairs
 enable-recursive-minibuffers   t
 etags-regen-mode               t
 fido-mode                      t
 flyspell-mode-line-string      nil
 flyspell-mode-map              nil
 fringe-mode                    8 ;; in pixel
 global-anzu-mode               t
 global-auto-revert-mode        t
 global-completion-preview-mode t
 global-hl-line-mode            nil
 global-page-break-lines-mode   t
 global-so-long-mode            t
 global-visual-line-mode        nil ;; wraps words long lines
 global-xref-mouse-mode         t
 icomplete-prospects-height     1
 icomplete-separator            (propertize " | " 'face '('bold :foreground "blue4"))
 icomplete-tidy-shadowed-file-names t
 indent-tabs-mode               nil
 indicate-empty-lines           t
 ispell-extra-args              '("--sug-mode=ultra")
 ispell-quietly                 t
 menu-bar-mode                  t
 package-autosuggest-mode       t
 pixel-scroll-mode              nil
 pixel-scroll-precision-mode    nil ;; see bug#69972
 project-mode-line              t
 recentf-mode                   t
 repeat-mode                    t
 require-final-newline t
 save-place-mode                t
 savehist-mode                  t
 scroll-bar-mode                'right ;; i like it
 show-paren-mode                t
 tool-bar-mode                  nil
 tooltip-mode                   nil
 undo-fu-session-global-mode    t
 use-short-answers              t
 vc-auto-revert-mode            t
 vhl/animation-style            'pulse
 volatile-highlights-mode       t
 vundo-compact-display          t
 vundo-glyph-alist              vundo-unicode-symbols
 which-key-mode                 t
 window-combination-resize      t
 winner-mode                    t ;; C-c buffers layout history
 x-underline-at-descent-line    t
 )


(diminish 'anzu-mode)
(diminish 'auto-revert-mode)
(diminish 'beacon-mode)
(diminish 'completion-preview-mode)
(diminish 'eldoc-mode)
(diminish 'page-break-lines-mode)
(diminish 'rainbow-delimiters-mode " ")
(diminish 'rainbow-mode "  ")
(diminish 'volatile-highlights-mode)
(diminish 'which-key-mode)


(defalias 'yes-or-no-p 'y-or-n-p)

(setq-default frame-title-format (list '(:eval (format "GNU Emacs %s - " emacs-version))
                                 '(buffer-file-name "%f" "%b")))

(when scroll-bar-mode
  ;; this disable scroll on minibuffer
  (message "[config] scroll bar enabled and configured")
  (set-window-scroll-bars (minibuffer-window) nil nil nil nil 1))

(setq-default custom-file (concat user-emacs-directory "custom.el"))
(when (file-exists-p custom-file) (load-file custom-file))

;;; init.el ends here
