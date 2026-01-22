;;; init.el --- Main file      -*- lexical-binding: t; -*-
;;; Commentary:
;;; basic Emacs configuration, by thiagolopes

(message "[config] start init.el")
(add-hook 'after-init-hook (lambda () (message "[config] finish init.el")))

(when (version< emacs-version "29.0")
  (message "Your Emacs is old for this config. Please upgrade if possible."))

;; identify the system
(setq *is-a-mac* (eq system-type 'darwin))
(setq *is-a-linux* (or (eq system-type 'gnu/linux) (eq system-type 'linux)))


(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;; TODO deal to worker without internet for some reason, emergencial mode.
;; 3th lib 'no-littering' is not optional
(when (not (package-installed-p 'no-littering))
  (package-install 'no-littering))
(require 'no-littering)

(setq custom-file (concat user-emacs-directory "custom.el"))
(when (file-exists-p custom-file)
  (load-file custom-file))


(eval-and-compile
  (defun list-missing-pckgs ()
    "List all missing packages."
    (when (not (package-installed-p 'dash))
      (package-install 'dash))
    (require 'dash)
    (-filter
     (-not #'package-installed-p)
     package-selected-packages))

  (let ((packages-not-installed (list-missing-pckgs)))
    (when packages-not-installed
      (-each packages-not-installed #'package-install)
      ;; OVERKILL REACTION
      (restart-emacs))))


(defalias 'yes-or-no-p 'y-or-n-p)

(fringe-mode        20) ;; in pixel
(tool-bar-mode     -1)
(menu-bar-mode     -1)
(scroll-bar-mode   -1)

(setq frame-title-format
      (list '(buffer-file-name "%f" "%b")
            '(:eval (format " - GNU Emacs %s" emacs-version))))

(when scroll-bar-mode
  ;; this disable scroll on minibuffer
  (message "[config] scroll bar enabled and configured")
  (set-window-scroll-bars (minibuffer-window) nil nil nil nil 1))

(prefer-coding-system 'utf-8)

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



;; ediff "d" now merge two diff without markers
(defun ediff-copy-both-to-C ()
  (interactive)
  (ediff-copy-diff ediff-current-difference nil 'C nil
                   (concat
                    (ediff-get-region-contents ediff-current-difference 'A ediff-control-buffer)
                    (ediff-get-region-contents ediff-current-difference 'B ediff-control-buffer))))
(defun add-d-to-ediff-mode-map () (define-key ediff-mode-map "d" 'ediff-copy-both-to-C))
(add-hook 'ediff-keymap-setup-hook 'add-d-to-ediff-mode-map)


(global-set-key (kbd "M-o")         'other-window)
(global-set-key (kbd "C-x k")       'kill-current-buffer) ;; kill without ask
(global-set-key (kbd "C-c k")       'kill-buffer)
(global-set-key (kbd "C-x C-b")     'ibuffer)             ;; visualize all buffers on menu
(global-set-key (kbd "M-!")         'async-shell-command)
(global-set-key (kbd "M-&")         'shell-command)
(global-set-key (kbd "M-u")         'upcase-dwim)
(global-set-key (kbd "M-l")         'downcase-dwim)
(global-set-key (kbd "M-c")         'capitalize-dwim)
(global-set-key (kbd "C-x C-d")     'dired)
(global-set-key (kbd "C-,")         'duplicate-line)
(global-set-key (kbd "<f9>")        (lambda ()
                                      (interactive)
                                      (if (project-current)
                                          (project-compile)
                                        (call-interactively #'compile))))
(global-set-key (kbd "S-C-<left>")  'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>")  'shrink-window)
(global-set-key (kbd "S-C-<up>")    'enlarge-window)


;; dired
(put 'dired-find-alternate-file 'disabled nil)


;; mode-line
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
                ;; from https://github.com/grolongo/nerd-icons-mode-line/blob/master/nerd-icons-mode-line.el#L50
                (:propertize
                 (:eval
                  (with-current-buffer (current-buffer)
                    (nerd-icons-icon-for-buffer)))
                 display (raise 0.1))
                " "
                mode-line-buffer-identification
                (:eval
                 (when-let ((project (project-current)))
                   '((project-mode-line project-mode-line-format)
                     (when vc-mode
                       (vc-mode vc-mode)))))
                " "
                mode-line-modes mode-line-misc-info
                mode-line-end-spaces
                mode-line-format-right-align
                mode-line-position
                "    ["mode-line-percent-position"]     "
                ))


(savehist-mode t)
(save-place-mode t)
(recentf-mode t)
(fido-vertical-mode t)



(when *is-a-mac*
  ;; do not resize on scroll
  ;; https://xenodium.com/hey-mouse-dont-mess-with-my-emacs-font-size
  (global-set-key (kbd "<pinch>") 'ignore)
  (global-set-key (kbd "<C-wheel-up>") 'ignore)
  (global-set-key (kbd "<C-wheel-down>") 'ignore)

  (setq mac-control-modifier 'control)
  (setq mac-command-modifier 'meta)
  (setq mac-right-option-modifier 'control))

(defun kill-all-buffers ()
  "Kill all open buffers."
  (interactive)
  (dolist (buf (buffer-list))
    (unless (string= (buffer-name buf) "*scratch*")
      (kill-buffer buf))))

(defun bool-to-int (bool)
  (if bool 1 0))


(use-package windmove
  :config
  (windmove-default-keybindings))

(use-package icomplete
  :hook
  (icomplete-minibuffer-setup . (lambda () (setq truncate-lines t))))

(use-package ansi-color
  :hook
  (compilation-filter . ansi-color-compilation-filter))

(use-package eldoc
  :init
  (global-eldoc-mode t)
  :diminish
  :custom
  (eldoc-echo-area-use-multiline-p t)
  (eldoc-documentation-compose 'eldoc-documentation-compose-eagerly))

(use-package flymake
  :custom
  (flymake-mode-line-lighter " ")
  (flymake-indicator-type 'margins)
  (flymake-margin-indicator-position 'right-margin)
  :hook
  (prog-mode . flymake-mode))

(use-package completion-preview
  :diminish
  :config
  (global-completion-preview-mode t))

(use-package which-key :diminish)

(require 'uniquify)

;; 3party
(global-set-key [remap goto-line] 'goto-line-preview)
(global-set-key (kbd "C-x /")     'goto-last-change)

(use-package jinx
  :diminish "  "
  ;; this is a commentary in English, this is a error: banama
  ;; este é um comentário em português, isso é um erro: banama
  :custom
  (jinx-languages "en pt_BR")
  :hook
  (emacs-startup . global-jinx-mode))

(use-package mode-line-bell
  :config
  (mode-line-bell-mode))

(use-package dumb-jump
  :custom
  (dumb-jump-prefer-searcher 'rg)
  (xref-show-definitions-function #'xref-show-definitions-completing-read)
  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  (add-hook 'dumb-jump-after-jump-hook 'better-jumper-set-jump))

(use-package rainbow-mode
  :diminish "  ")

(use-package rainbow-delimiters
  :init
  (setq global-rainbow-delimiters-active t)
  (defun global-rainbow-delimiters-mode () (interactive)

         (setq global-rainbow-delimiters-active (not global-rainbow-delimiters-active))

         (dolist (buffer (buffer-list))
           (with-current-buffer buffer
               (rainbow-delimiters-mode (bool-to-int global-rainbow-delimiters-active))))

         (if global-rainbow-delimiters-active
             (progn
               (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
               (message "Global Rainbow Delimiters: [ON]"))
           (progn
             (remove-hook 'prog-mode-hook 'rainbow-delimiters-mode)
             (message "Global Rainbow Delimiters: [OFF]"))))
  :diminish " "
  :hook (prog-mode . rainbow-delimiters-mode)
  :bind
  ("<f5>" . global-rainbow-delimiters-mode))

(use-package org-modern
  :hook
  (org-mode . org-modern-mode)
  (org-mode . visual-line-mode)
  ;; (org-mode . variable-pitch-mode)
  (org-mode . auto-fill-mode)
  (org-mode . verb-mode)
  :custom
  (org-hide-emphasis-markers t)
  (org-modern-fold-stars
   '(("▶" . "▼")
     ("▷▷" . "▽▽")
     ("⯈⯈⯈" . "⯆⯆⯆")
     ("▹▹▹▹" . "▿▿▿▿")
     ("▸▸▸▸▸" . "▾▾▾▾▾"))))

(use-package org-appear
  :hook
  (org-mode . org-appear-mode))

(use-package consult
  :config
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep consult-man
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.8 any))
  ;; :custom
  ;; (completion-in-region-function #'consult-completion-in-region)
  ;; :hook
  ;; (completion-list-mode . consult-preview-at-point-mode)
  :bind (("M-y"     . consult-yank-pop)
         ("C-x b"   . consult-buffer)
         ("C-c p"   . consult-project-buffer)
         ("C-c m"   . consult-global-mark)
         ("C-c f"   . consult-find)
         ("C-c i"   . consult-imenu)
         ("C-c r"   . consult-ripgrep)
         ("C-c l"   . consult-line-multi)
         ("M-g g"   . consult-goto-line)
         ("M-g M-g" . consult-goto-line)))

(use-package multiple-cursors
  :custom-face
  (mc/cursor-bar-face ((t (:background "chartreuse" :foreground "#ffffff" :height 1))))
  (mc/cursor-face ((t (:foreground "chartreuse"))))
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
  :config
  (push "*Warnings*" popwin:special-display-config)
  (popwin-mode t))

(use-package helpful
  :bind
  ("C-h f" . helpful-callable)
  ("C-h v" . helpful-variable)
  ("C-h k" . helpful-key)
  ("C-h x" . helpful-command))

(use-package marginalia
  :config
  (setq marginalia-align 'left)
  :init
  (marginalia-mode t))

(use-package treemacs
  :config
  (treemacs-follow-mode t)
  (treemacs-tag-follow-mode t)
  (treemacs-project-follow-mode t)
  :custom
  (treemacs-position 'right)
  (treemacs-deferred-git-apply-delay 5)
  (treemacs-git-mode 'simple)
  :bind
  ("C-c t" . treemacs))

(use-package corfu
  ;; :custom-face
  ;; (corfu-default ((t (:inherit default))))
  :init
  (global-corfu-mode t)
  (corfu-history-mode t)
  (corfu-popupinfo-mode t)
  :config
  ;; Option 1: Unbind RET completely
  (keymap-unset corfu-map "RET")
  :custom
  ;; (corfu-auto t)
  (corfu-popupinfo-delay 1)
  (corfu-preselect 'directory)
  :bind (:map corfu-map
              ("C-n" . corfu-next)
              ("C-p" . corfu-previous)
              ("<escape>" . corfu-quit)
              ;; ("<return>" . corfu-insert)
              ("M-d" . corfu-show-documentation)
              ("M-l" . corfu-show-location)))

(use-package corfu-terminal
  :if 'display-graphical-p
  :after corfu
  :init
  (corfu-terminal-mode))

(use-package cape
  :init
  (add-hook 'completion-at-point-functions #'cape-keyword)
  (add-hook 'completion-at-point-functions #'cape-abbrev)
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-block)
  (add-hook 'completion-at-point-functions #'cape-dict)
  (add-hook 'completion-at-point-functions #'cape-history))

(use-package anzu
  :diminish
  :init
  (global-anzu-mode))

(use-package visual-fill-column
  :disabled
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

(use-package expreg
  :bind ("M-@" . expreg-expand))

(use-package rg
  :ensure-system-package rg
  :custom
  (rg-keymap-prefix "\C-cs")
  :init
  (rg-enable-default-bindings))

;; (use-package buffer-name-relative
;;   :init (buffer-name-relative-mode))

(use-package exec-path-from-shell
  :init (exec-path-from-shell-initialize))

(use-package super-save
  :diminish
  :config
  (super-save-mode t))

(use-package symbol-overlay
  :bind
  ("<f6>" . symbol-overlay-put))

(use-package move-dup
  :bind
  (("M-<up>" . move-dup-move-lines-up)
   ("M-<down>" . move-dup-move-lines-down)))

;; (use-package eldoc-box
;;   :diminish
;;   :config
;;   ;; stolen from https://gitlab.com/sadiq/dotfiles/-/commit/fa8db2dacb304f568fde8d69a1c51e879f03dde8
;;   (defun custom-eldoc-box-at-point-position (width height)
;;     (let* ((point-pos (eldoc-box--point-position-relative-to-native-frame))
;;            ;; calculate point coordinate relative to native frame
;;            ;; because childframe coordinate is relative to native frame
;;            (x (car point-pos))
;;            (y (cdr point-pos)))
;;       (cons
;;        ;; Try keeping the box at the right with an offset of 10 pixel
;;        (max 0 (min (+ x 10) (- (frame-inner-width) width 10)))
;;        ;; Try keeping the box at the top with an offset of 15 pixel
;;        ;; Also add a smaller offset of 5 if the box is too small
;;        (max 0 (- y height (if (<= height (* 3 (frame-char-height))) 5 15))))))
;;   (defun custom-eldoc-box-corner-position-function (width height)
;;     (let* ((w-size (window-body-pixel-edges))
;;            (w-b (nth 3 w-size))
;;            (w-r (nth 0 w-size))
;;            (w-w (window-pixel-width))
;;            (point-pos (eldoc-box--point-position-relative-to-native-frame))
;;            (x (car point-pos))
;;            (y (car point-pos)))
;;     (cons x y)))
;;   :custom
;;   (eldoc-box-lighter "")
;;   (eldoc-box-clear-with-C-g t)
;;   (eldoc-box-position-function #'eldoc-box--default-upper-corner-position-function)
;;   ;; (eldoc-box-position-function #'custom-eldoc-box-corner-position-function)
;;   ;; (eldoc-box-position-function #'custom-eldoc-box-at-point-position)
;;   ;; (eldoc-box-offset '(16 130 16))
;;   :hook
;;   (eglot-managed-mode . eldoc-box-hover-mode)
;;   (prog-mode . eldoc-box-hover-mode))

(use-package prism
  :bind
  ;; TODO update to python use prism-whitespace-mode
  ("<f7>" . prism-mode))


(use-package nerd-icons)
(use-package nerd-icons-dired
  :after nerd-icons
  :hook
  (dired-mode . nerd-icons-dired-mode))
(use-package treemacs-nerd-icons
  :after (nerd-icons treemacs)
  :config
  (treemacs-load-theme "nerd-icons"))
(use-package nerd-icons-ibuffer
  :after nerd-icons
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode))
(use-package nerd-icons-completion
  :after marginalia
  :after nerd-icons
  :config
  (nerd-icons-completion-mode)
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))
(use-package nerd-icons-corfu
  :after (corfu nerd-icons)
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))


(use-package flyover
  :diminish "[ Overlay Errors]"
  :custom
  (flyover-use-theme-colors t)
  :custom-face
  (flyover-marker
   ((t :inherit default
       :foreground "black"
       :style 'line)))
  :bind
  ("<f8>". flyover-mode))

(use-package page-break-lines
  :diminish
  :custom
  (page-break-lines-max-width 79)
  :config
  (global-page-break-lines-mode t))

(use-package gruber-darker-theme
  :custom-face
  ;; (rainbow-delimiters-depth-1-face ((t (:foreground "#AE81FF"))))
  ;; (rainbow-delimiters-depth-2-face ((t (:foreground "#66D9EF"))))
  ;; (rainbow-delimiters-depth-3-face ((t (:foreground "#A6E22E"))))
  ;; (rainbow-delimiters-depth-4-face ((t (:foreground "#E6DB74"))))
  ;; (rainbow-delimiters-depth-5-face ((t (:foreground "#FD971F"))))
  ;; (rainbow-delimiters-depth-6-face ((t (:foreground "#F92672"))))
  ;; (rainbow-delimiters-depth-7-face ((t (:foreground "#AE81FF"))))
  ;; (rainbow-delimiters-depth-8-face ((t (:foreground "#66D9EF"))))
  ;; (rainbow-delimiters-depth-9-face ((t (:foreground "#A6E22E"))))
  ;; (rainbow-delimiters-depth-10-face ((t (:foreground "#E6DB74"))))
  ;; (rainbow-delimiters-depth-11-face ((t (:foreground "#FD971F"))))
  ;; (rainbow-delimiters-depth-12-face ((t (:foreground "#F92672"))))
  (note    ((t :foreground "#73c936")))
  (warning ((t :foreground "#ffdd33")))
  (error   ((t :foreground "#c73c3f")))
  (flymake-error
   ((((supports :underline (:style wave)))
     (:underline (:style wave :color "#c73c3f")
                 :foreground unspecified
                 :background unspecified
                 :inherit unspecified))
    (t (:foreground "#c73c3f" :weight bold :underline t))))
  (flymake-warning
   ((((supports :underline (:style wave)))
     (:underline (:style wave :color "#ffdd33")
                 :foreground unspecified
                 :background unspecified
                 :inherit unspecified))
    (t (:forground "#ffdd33" :weight bold :underline t))))
  (flymake-note
   ((((supports :underline (:style wave)))
     (:underline (:style wave :color "#73c936")
                 :foreground unspecified
                 :background unspecified
                 :inherit unspecified))
    (t (:forground "#73c936" :weight bold :underline t))))
  (mode-line-inactive ((t :background "#181818")))
  (mode-line-buffer-id ((t :background nil))))

(use-package vundo
  :custom
  (vundo-window-max-height 5)
  (vundo-glyph-alist vundo-unicode-symbols)
  :bind ("C-x C-/". vundo))

(use-package embark
  :config
  (setq embark-verbose-indicator-display-action '(display-buffer-at-bottom
                                                  (window-height . fit-window-to-buffer)))
  :bind
  (("C-." . embark-act)
   ("M-." . embark-dwim)))
(use-package embark-consult
  :after consult
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package web-mode
  :mode ("\\.html\\'" . web-mode))

(use-package eglot
  :hook
  (prog-mode . eglot-ensure)
  :custom
  (eglot-autoshutdown t)
  (eglot-extend-to-xref t)
  (eglot-ignored-server-capabilities '(:codeLensProvider
                                       :documentOnTypeFormattingProvider
                                       :foldingRangeProvider
                                       :inlayHintProvider)))

(use-package deno-ts-mode
  :after eglot
  :config
  (put 'deno-ts-mode 'eglot-language-id "typescript"))

(use-package yafolding
  :hook (prog-mode . yafolding-mode))

(use-package smartscan
  :hook (prog-mode . smartscan-mode))

(use-package verb
  :custom
  (verb-auto-kill-response-buffers t))

(use-package diff-hl
  :config
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  (let* ((width 2)
         (bitmap (vector (1- (expt 2 width)))))
    (define-fringe-bitmap 'my:diff-hl-bitmap bitmap 1 width '(top t)))
  (setq diff-hl-fringe-bmp-function (lambda (type pos) 'my:diff-hl-bitmap))
  (global-diff-hl-mode)
  (diff-hl-dired-mode t)
  (diff-hl-flydiff-mode t))

(use-package pdf-tools
  :config
  (setq pdf-view-display-size 'fit-page)
  (add-hook 'pdf-view-mode-hook #'pdf-view-roll-minor-mode))

(provide 'init)
;;; init.el ends here
