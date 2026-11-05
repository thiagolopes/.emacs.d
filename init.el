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

(setopt user-full-name "Thiago Lopes"
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
   cape
   consult
   diff-hl
   diminish
   dumb-jump
   eglot
   embark
   embark-consult
   exec-path-from-shell
   expand-region
   popwin
   flycheck
   prism
   helpful
   git-link
   git-timemachine
   goto-last-change
   goto-line-preview
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
   rainbow-delimiters
   rainbow-mode
   rg
   sudo-edit
   undo-fu
   undo-fu-session
   uuidgen
   verb
   vundo
   yafolding
   zzz-to-char

   ;; Themes
   ef-themes
   gruber-darker-theme
   modus-themes
   purp-theme
   srcery-theme
   standard-themes

   ;; File Modes
   fish-mode
   i3wm-config-mode
   cmake-mode
   deno-ts-mode
   dockerfile-mode
   go-mode
   hledger-mode
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

;; (global-set-key (kbd "M-l")         'downcase-dwim)
;; (global-set-key (kbd "M-c")         'capitalize-dwim)

(global-set-key (kbd "M-<up>"  ) 'move-dup-move-lines-up)
(global-set-key (kbd "M-<down>") 'move-dup-move-lines-down)

(global-set-key (kbd "M-z")   #'zzz-to-char)
(global-set-key (kbd "<f9>")  (lambda () (interactive)
                                "Run compile on project if available."
                                (if (project-current) (project-compile)
                                  (call-interactively #'compile))))

(defun flycheck-configure ()
  "Configure flycheck."
  (progn
    (global-flycheck-mode 1)
    (global-flycheck-eglot-mode 1)
    ;; (setopt flycheck-indication-mode 'left-margin) ;; this use margin instead fringe
    (setopt flycheck-annotate-mode 'oel)
    (setopt flymake-start-on-save-buffer nil)
    (setopt flymake-start-on-flymake-mode nil)
    (setopt flycheck-highlighting-style '(delimiters "" ""))
    (setopt flycheck-mode-line-prefix " ")))

(defun org-mode-configure ()
  "Configure Org."
  (progn
    (add-hook 'org-mode-hook #'visual-line-mode)
    (add-hook 'org-mode-hook #'org-appear-mode)
    (setopt org-return-follows-link t
            org-startup-folded 'fold
            org-hide-emphasis-markers t)
    ;; TODO use envvar
    (setopt org-roam-directory (file-truename "~/doc/Notas"))))

(defun dump-jump-configure ()
  "Configure Dump Jump."
  (progn
    ;; enable dump-jump as a option in xref-backend-functions
    (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
    (setopt dumb-jump-prefer-searcher 'rg)
    ;; consult required
    (setopt xref-show-definitions-function #'consult-xref)))

(defun diff-hl-configure ()
  "Configure lateral diff on buffers."
  (progn
    (add-hook 'prog-mode-hook #'diff-hl-mode)
    ;; (add-hook 'prog-mode-hook #'diff-hl-margin-mode)
    (add-hook 'dired-mode-hook #'diff-hl-dired-mode)
    (setopt diff-hl-disable-on-remote t)
    (setopt diff-hl-fringe-bmp-function 'diff-hl-fringe-bmp-from-type)
    (custom-set-faces
     '(diff-hl-change ((t (:inherit diff-changed))))
     '(diff-hl-delete ((t (:inherit diff-removed))))
     '(diff-hl-insert ((t (:inherit diff-added)))))))

(defun helpful-configure ()
  "Switch all describe-* to use helpful."
  (progn
    (advice-add 'describe-function :override #'helpful-function)
    (advice-add 'describe-command  :override #'helpful-command)
    (advice-add 'describe-key      :override #'helpful-key)
    (advice-add 'describe-variable :override #'helpful-variable)
    (advice-add 'describe-symbol   :override #'helpful-symbol)))

(defun whitespace-configure ()
  "Configure whitespace."
  (progn
    (setopt whitespace-style '(face spaces empty tabs newline trailing space-mark tab-mark))
    (add-hook 'before-save-hook #'whitespace-cleanup)))

(defun popwin-configure ()
  "Configure popwin - avoid bottom buffers to close on C-g or loose focus."
  (progn
    (popwin-mode t)
    (push "*Warnings*" popwin:special-display-config)
    (push "*Flycheck errors*" popwin:special-display-config)))

(diff-hl-configure)
(dump-jump-configure)
(flycheck-configure)
(helpful-configure)
(org-mode-configure)
(popwin-configure)
(whitespace-configure)


(exec-path-from-shell-initialize) ;; load PATH
(prefer-coding-system 'utf-8)
(windmove-default-keybindings) ;; moviment with shift+arrow


(add-hook 'emacs-startup-hook
          (lambda ()
            (progn
              (split-window-right)
              (switch-to-buffer "*scratch*"))))

(add-hook 'prog-mode-hook 'hl-todo-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

(add-hook 'text-mode-hook 'flyspell-mode)

(add-hook 'hledger-mode-hook (lambda () (whitespace-mode)))

(add-hook 'completion-at-point-functions #'cape-keyword)
(add-hook 'completion-at-point-functions #'cape-abbrev)
(add-hook 'completion-at-point-functions #'cape-dabbrev)
(add-hook 'completion-at-point-functions #'cape-file)
(add-hook 'completion-at-point-functions #'cape-elisp-block)
(add-hook 'completion-at-point-functions #'cape-dict)
(add-hook 'completion-at-point-functions #'cape-history)


(setopt eglot-autoshutdown t
        eglot-extend-to-xref t
        eglot-ignored-server-capabilities '(:codeLensProvider
					    :hoverProvider
					    :documentOnTypeFormattingProvider
					    :foldingRangeProvider :inlayHintProvider))

(setopt font-use-system-font t
        frame-resize-pixelwise t
        window-resize-pixelwise t
        mode-line-compact 'long)

(setopt save-interprogram-paste-before-kill t
        mouse-yank-at-point t
        mouse-drag-and-drop-region t
        mouse-drag-and-drop-region-cross-program t
        mouse-drag-mode-line-buffer t)

(setopt imenu-auto-rescan t
	view-read-only t)

(setopt vc-deduce-backend-nonvc-modes t
	vc-dir-save-some-buffers-on-revert t
	vc-find-revision-no-save t
	vc-follow-symlinks t
	vc-use-incoming-outgoing-prefixes t)

(setopt minibuffer-visible-completions t
	completions-detailed t
	completions-group t
	completion-auto-select 'second-tab
	completion-eager-update t
	global-completion-preview-mode t
	tab-always-indent 'complete
	package-menu-use-current-if-no-marks nil
	frame-inhibit-implied-resize t
	compilation-scroll-output 'first-error
        compilation-always-kill t)

(setopt ef-dream-palette-overrides
        '((bg-main               "#131015")
          (bg-hl-line            "#232224")
          (fg-mode-line-active   "#f2ddcf")
          (bg-mode-line-active   "#472b00")
          (yellow-cooler         "#ff9f0a")
          (bg-hl-line            "#2e1a3a")
          (bg-hl-line            "#352102")
          (bg-hl-line            "#3b393e")
          (bg-mode-line-inactive "#5E4527")))


(setopt treesit-enabled-modes t
        treesit-auto-install-grammar 'always)

(setopt display-line-numbers-type 'relative) ;; TODO implement linum relative in goto-line-preview-relative

(setopt dired-kill-when-opening-new-dired-buffer t
        dired-listing-switches "-alh"
	dired-auto-revert-buffer t
	dired-mouse-drag-files t)

(setopt c-basic-offset 4
        c-ts-mode-indent-offset 4)

(setopt
  ;; completion-auto-help nil
 ;; custom-buffer-sort-alphabetically t
 ;; display-line-numbers-width 4
 ;; enable-recursive-minibuffers t
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
 ace-jump-mode-case-fold        t
 backup-by-copying              t
 blink-cursor-mode              t
 column-number-mode             t
 context-menu-mode              t ;; enable menu with mouse right click
 cursor-type                    'box
 custom-buffer-indent           4
 custom-safe-themes             t
 delete-selection-mode          t
 editorconfig-mode              t
 electric-pair-mode             nil ;; auto close pairs
 etags-regen-mode               t
 fido-mode                      t
 fringe-mode                    8 ;; in pixel
 global-anzu-mode               t
 global-auto-revert-mode        t
 global-completion-preview-mode t
 global-hl-line-mode            nil
 global-page-break-lines-mode   t
 global-so-long-mode            t
 global-visual-line-mode        nil ;; wraps words long lines
 global-xref-mouse-mode         t
 indent-tabs-mode               nil
 indicate-empty-lines           t
 line-spacing                   0
 menu-bar-mode                  t
 flyspell-mode-map              nil
 flyspell-mode-line-string      nil
 package-autosuggest-mode       t
 pixel-scroll-mode              nil
 pixel-scroll-precision-mode    nil ;; see bug#69972
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
 which-key-mode                 t
 window-combination-resize      t
 winner-mode                    t ;; C-c buffers layout history
 x-underline-at-descent-line    t
 project-mode-line              t
 )


(diminish 'page-break-lines-mode)
(diminish 'eldoc-mode)
(diminish 'rainbow-mode "  ")
(diminish 'rainbow-delimiters-mode " ")
(diminish 'which-key-mode)
(diminish 'completion-preview-mode)
(diminish 'anzu-mode)
(diminish 'auto-revert-mode)


(defalias 'yes-or-no-p 'y-or-n-p)

(setopt frame-title-format (list '(:eval (format "GNU Emacs %s - " emacs-version))
                                 '(buffer-file-name "%f" "%b")))

(when scroll-bar-mode
  ;; this disable scroll on minibuffer
  (message "[config] scroll bar enabled and configured")
  (set-window-scroll-bars (minibuffer-window) nil nil nil nil 1))

(setopt custom-file (concat user-emacs-directory "custom.el"))
(when (file-exists-p custom-file) (load-file custom-file))

;;; init.el ends here
