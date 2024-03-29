;;; early-init.el -- only focus on emacs setup -*- lexical-binding: t; -*-
;;; Commentary:
;;;  to everthing early
;;; Code:

(setq package-enable-at-startup nil)

(setq straight-use-package-by-default t
      straight-cache-autoloads t
      straight-vc-git-default-clone-depth 1
      straight-check-for-modifications '(find-when-checking)
      package-enable-at-startup nil
      vc-follow-symlinks t)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
(setq vc-follow-symlinks 'ask) ; restore default

(require 'straight-x)
(straight-use-package 'use-package)
(require 'use-package)
(use-package general)

(setq use-package-verbose t
      use-package-compute-statistics nil
      user-full-name "Thiago Lopes"
      user-mail-address "thiagolopes@protonmail.com"
      gc-cons-threshold 100000000
      read-process-output-max (* 1024 1024)
      comp-deferred-compilation t)

(defvar comp-deferred-compliation)

(add-hook 'after-init-hook #'garbage-collect t)

(custom-set-faces
 '(default ((t (:height 135 :family "Iosevka Comfy" :weight normal)))))

;; Add padding inside frames (windows)
(add-to-list 'default-frame-alist '(internal-border-width . 2))

(set-window-buffer nil (current-buffer))

(add-hook 'before-save-hook 'whitespace-cleanup)
(setq whitespace-line-column nil
      whitespace-style
      '(face indentation tabs tab-mark spaces space-mark newline newline-mark
             trailing lines-tail)
      whitespace-display-mappings
      '((tab-mark ?\t [?› ?\t])
        (newline-mark ?\n [?¬ ?\n])
        (space-mark ?\  [?·] [?.])))
(context-menu-mode t)
(column-number-mode t)
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

(setq site-run-file nil
      inhibit-compacting-font-caches t)
(when (boundp 'read-process-output-max)
  ;; 1MB in bytes, default 4096 bytes
  (setq read-process-output-max 1048576))

(setq enable-recursive-minibuffers t)
;; Show current key-sequence in minibuffer ala 'set showcmd' in vim. Any
;; feedback after typing is better UX than no feedback at all.
(setq echo-keystrokes 0.02)
;; Expand the minibuffer to fit multi-line text displayed in the echo-area. This
;; doesn't look too great with direnv, however...
(setq resize-mini-windows 'grow-only)
;; Try to keep the cursor out of the read-only portions of the minibuffer.
(setq minibuffer-prompt-properties '(read-only t intangible t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

(setq x-select-enable-clipboard-manager nil
      warning-minimum-level :error)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(horizontal-scroll-bar-mode -1)
(menu-bar-mode -1)
(defalias 'yes-or-no-p 'y-or-n-p)
(show-paren-mode 1)

(setq show-paren-highlight-openparen nil
      save-interprogram-paste-before-kill t
      apropos-do-all t
      mouse-yank-at-point t
      require-final-newline t
      backup-by-copying t
      frame-inhibit-implied-resize t
      ediff-window-setup-function 'ediff-setup-windows-plain)
(setq make-backup-files nil)

;; A simple frame title
(setq frame-title-format '("%b – Emacs")
      icon-title-format frame-title-format)

;; Don't resize the frames in steps; it looks weird, especially in tiling window
;; managers, where it can leave unseemly gaps.
(setq frame-resize-pixelwise t)

;; But do not resize windows pixelwise, this can cause crashes in some cases
;; when resizing too many windows at once or rapidly.
(setq window-resize-pixelwise t)

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
(require 'uniquify)
(setq confirm-nonexistent-file-or-buffer nil
      uniquify-buffer-name-style 'forward)

;; no beeping or blinking please
;; (setq ring-bell-function #'ignore
;; visible-bell nil)
;; beeping PLEASE! but not too much
(setq ring-bell-function
      (lambda ()
        (unless (memq this-command
                      '(isearch-abort
                        abort-recursive-edit
                        exit-minibuffer
                        keyboard-quit
                        previous-line
                        next-line
                        scroll-down
                        scroll-up
                        cua-scroll-down
                        cua-scroll-up))
          (ding))))

;; middle-click paste at point, not at click
(setq mouse-yank-at-point t)

(blink-cursor-mode -1)
(setq-default cursor-type 'bar
              cursor-in-non-selected-windows nil)
(setq blink-matching-paren nil
      x-stretch-cursor nil)
;; useful information, like git-gutter and flycheck.
(setq indicate-buffer-boundaries nil
      indicate-empty-lines nil)

;; Explicitly define a width to reduce the cost of on-the-fly computation
(setq-default display-line-numbers-width 3)
;; Show absolute line numbers for narrowed regions to make it easier to tell the
;; buffer is narrowed, and where you are, exactly.
(setq-default display-line-numbers-widen t)
;; (add-hook 'prog-mode-hook #'display-line-number-mode)
;; (add-hook 'text-mode-hook #'display-line-number-mode)
;; (add-hook 'conf-mode-hook #'display-line-number-mode)
;; (setq display-line-numbers-type t) ;; disable line number as default
(general-define-key "<f10>" 'global-display-line-numbers-mode)

(setq visual-line-fringe-indicators t)

(electric-pair-mode t)

(require 'saveplace)
(setq-default save-place t)
(save-place-mode t)
(setq save-place-file (concat user-emacs-directory "cache/places")
      cache-dir (concat user-emacs-directory "/cache")
      custom-file (concat user-emacs-directory "/custom.el")
      require-final-newline t
      load-prefer-newer t)

;; Made SHIFT+arrow to move to the next adjacent window in the specified direction
(windmove-default-keybindings)

(general-define-key "M-u" #'upcase-dwim
                    "M-l" #'downcase-dwim
                    "M-c" #'capitalize-dwim)

(general-define-key "<mouse-8>" #'previous-buffer
                    "<mouse-9>" #'next-buffer)

(general-define-key "C-=" #'text-scale-increase
                    "C-+" #'text-scale-increase
                    "C--" #'text-scale-decrease
                    "M-n" #'forward-paragraph
                    "M-p" #'backward-paragraph)

(general-define-key "<f6>" #'font-lock-mode)

(general-define-key "M-3" '(lambda () (interactive) (insert "#"))
                    "M-9" '(lambda () (interactive) (insert "("))
                    "M-0" '(lambda () (interactive) (insert ")"))
                    "M-[" '(lambda () (interactive) (insert "{"))
                    "M-]" '(lambda () (interactive) (insert "}")))

(general-define-key "C-x C-b" 'ibuffer
                    "C-c p"   'find-file-at-point
                    "M-j"     'join-line)

(setq isearch-lax-whitespace t
      isearch-regexp-lax-whitespace t
      search-whitespace-regexp "[ \t\r\n]+")

;; Yes, I really want to quit.
(setq confirm-kill-emacs nil)

;; Increase minibuffer font
;; (add-hook 'minibuffer-setup-hook
;;           '(lambda () (set (make-local-variable 'face-remapping-alist)
;;                       '((default :height 1.4)))))

;; Setup dark GTK theme if available
(defun set-emacs-frames-gtk (variant)
  (dolist (frame (frame-list))
    (let* ((window-id (frame-parameter frame 'outer-window-id))
           (id (string-to-number window-id))
           (cmd (format "xprop -id 0x%x -f _GTK_THEME_VARIANT 8u -set _GTK_THEME_VARIANT \"%s\"" id
                        variant)))
      (call-process-shell-command cmd))))
(when (and (eq system-type 'gnu/linux) (display-graphic-p))
  (set-emacs-frames-gtk "dark"))

;; Imortal *scratch* !!
(defun immortal-scratch ()
  (if (eq (current-buffer) (get-buffer "*scratch*"))
      (progn (bury-buffer)
             nil) t))
(defun save-persistent-scratch ()
  "Save the contents of *scratch*"
  (with-current-buffer (get-buffer-create "*scratch*")
    (write-region (point-min) (point-max)
                  (concat user-emacs-directory "scratch"))))
(defun load-persistent-scratch ()
  "Reload the scratch buffer"
  (let ((scratch-file (concat user-emacs-directory "scratch")))
    (if (file-exists-p scratch-file)
        (with-current-buffer (get-buffer "*scratch*")
          (delete-region (point-min) (point-max))
          (insert-file-contents scratch-file)))))
(add-hook 'kill-buffer-query-functions 'immortal-scratch)
(add-hook 'after-init-hook 'load-persistent-scratch)
(add-hook 'kill-emacs-hook 'save-persistent-scratch)
(run-with-idle-timer 300 t 'save-persistent-scratch)

;; Yes, I really want compile
(setq compilation-ask-about-save nil)

;; ctyle
(setq-default indent-tabs-mode nil
              tab-width 4
              c-set-style "k&r"
              c-basic-offset 4
              comment-style 'extra-line)
(add-hook 'c-mode-hook '(lambda () (setq comment-start "//"
                                    comment-end   "")))
(add-hook 'prog-mode #'electric-operator-mode)

;; copy-paste
(cua-mode t)

;; treat camel-cased words as individual words.
(add-hook 'prog-mode-hook 'subword-mode)
;; don't assume sentences end with two spaces after a period.
(setq sentence-end-double-space nil)
;; show matching parens
(defun show-paren--locate-near-paren-ad ()
  "Locate an unescaped paren \"near\" point to show.
If one is found, return the cons (DIR . OUTSIDE), where DIR is 1
for an open paren, -1 for a close paren, and OUTSIDE is the buffer
position of the outside of the paren.  Otherwise return nil."
  (let* ((before (show-paren--categorize-paren (point))))
    (when (or
           (eq (car before) 1)
           (eq (car before) -1))
      before)))
;; (advice-add 'show-paren--locate-near-paren :override #'show-paren--locate-near-paren-ad)
(show-paren-mode t)
(setq show-paren-delay 0.0)
;; limit files to 80 columns. Controversial, I know.
(setq-default fill-column 80)
;; handle very long lines without hurting emacs
(global-so-long-mode)

(setq default-directory "~/"
      ;; overwrite text when selected, like we expect.
      delete-seleciton-mode t
      ;; quiet startup
      inhibit-startup-message t
      ;; hopefully all themes we install are safe
      custom-safe-themes t
      ;; simple lock/backup file management
      create-lockfiles nil
      backup-by-copying t
      delete-old-versions t
      ;; when quiting emacs, just kill processes
      confirm-kill-processes nil
      ;; ask if local variables are safe once.
      enable-local-variables t
      ;; life is too short to type yes or no
      use-short-answers t
      ;; clean up dired buffers
      dired-kill-when-opening-new-dired-buffer t)

(setq-default dired-listing-switches "-alh")
(require 'ls-lisp)
(setq ls-lisp-dirs-first t)
(setq ls-lisp-use-insert-directory-program nil)

;; always highlight code
(global-font-lock-mode 1)
;; refresh a buffer if changed on disk
(setq global-auto-revert-non-file-buffers t)
(global-auto-revert-mode 1)

;; utf8
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(set-file-name-coding-system 'utf-8)
(set-clipboard-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)

(add-hook 'compilation-filter-hook 'ansi-color-compilation-filter)

;; default browser
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "firefox")

;; modeline which function
(which-function-mode -1)
(setq x-underline-at-descent-line t)

;; enable fringe mode
(fringe-mode)
(general-setq-default indicate-empty-lines t
                      indicate-buffer-boundaries 'left)

;; eletric-mode disabled
(electric-indent-mode -1)

;; Hippie-expand
(hippie-expand t)
(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        ;; try-expand-dabbrev-from-kill
        ;; try-expand-all-abbrevs
        ;; try-expand-list
        ;; try-expand-lien
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol
        try-complete-file-name-partially
        try-complete-file-name))
(global-set-key [remap dabbrev-expand] 'hippie-expand)

;; Duplicate line - stolen from rexim
(defun duplicate-line ()
  "Duplicate current line"
  (interactive)
  (let ((column (- (point) (point-at-bol)))
        (line (let ((s (thing-at-point 'line t)))
                (if s (string-remove-suffix "\n" s) ""))))
    (move-end-of-line 1)
    (newline)
    (insert line)
    (move-beginning-of-line 1)
    (forward-char column)))
(general-define-key "C-," 'duplicate-line)

;;
(follow-mode 1)
;;
(setq view-read-only t)

;; FIXME finish mode-line
;; https://protesilaos.com/codelog/2023-07-29-emacs-custom-modeline-tutorial/
;; (defun buffer-name-modeline ()
;;   (format " %s  " (buffer-name)))
;; (defun buffer-modified-modeline ()
;;   (when (buffer-modified-p)
;;     (format " %s")))

;; (defvar-local my-modeline-modified
;;   '(:eval
;;     (when (mode-line-window-selected-p)
;;       (propertize (buffer-name-modeline) 'face 'font-lock-comment-delimiter-face))))
;; (put 'my-modeline-buffer-name 'risky-local-variable t)

;; (defvar-local my-modeline-minion-mode
;;   '(:eval
;;     (when (mode-line-window-selected-p)
;;       minions-mode-line-modes)))
;; (put 'my-modeline-minion-mode 'risky-local-variable t)

;; (setq-default mode-line-format
;;               '("%e"
;;                 my-modeline-buffer-name
;;                 my-modeline-minion-mode))
(setq-default mode-line-format
              '("%e" mode-line-front-space
                (:propertize
                 ("" mode-line-mule-info mode-line-client mode-line-modified
                  mode-line-remote)
                 display (min-width (1.0)))
                mode-line-frame-identification
                mode-line-buffer-identification
                " - "
                minions-mode-line-modes
                mode-line-misc-info
                (vc-mode vc-mode) "  "
                mode-line-end-spaces))

;; Completion preview
(when (not (fboundp 'completion-preview-mode))
  (load "~/.emacs.d/completion-preview.el")
  (require 'completion-preview))
(add-hook 'prog-mode-hook #'completion-preview-mode)
(add-hook 'text-mode-hook #'completion-preview-mode)

;; eshell pop
(defun term-pop (fn name)
  (if (not (get-buffer-window name))
    (progn
      (split-window-below 10)
      ;; (other-window -1)
      (switch-to-buffer (next-buffer))
      (funcall fn))
    (if (equal (buffer-name) name)
        (delete-window)
        (switch-to-buffer-other-window name))))
(general-define-key "M-<f12>" '(lambda () (interactive) (term-pop 'eshell "*eshell*"))
                    "<f12>"   '(lambda () (interactive) (term-pop 'shell "*shell*")))

;; Load theme
(load-theme 'greenized)
;; (load-theme 'zenburned)

;; save only last buffer and window size
(desktop-save-mode 1)
(setq desktop-files-not-to-save "^$")
(defun clean-desktop-save ()
  (interactive)
  (progn
    (setq desktop-clear-preserve-buffers t)
    (desktop-save-in-desktop-dir)
    (setq desktop-clear-preserve-buffers nil)))
(add-hook 'kill-emacs-hook 'clean-desktop-save)

;;; early-init.el ends here;
