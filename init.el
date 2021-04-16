;;; init.el -*- lexical-binding: t; -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs Refined.					   ;;
;; - Usigin Sanemacs version 0.3.0 as template		   ;;
;; - References:					   ;;
;; -- spacemacs						   ;;
;; -- doomemacs						   ;;
;; -- purcell						   ;;
;; -- http://www.i3s.unice.fr/~malapert/emacs_orgmode.html ;;
;; -- https://github.com/manateelazycat/delete-block	   ;;
;; -- https://github.com/MatthewZMD/.emacs.d		   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Author:  Thiago Lopes <thiagolopes@pm.me>
;; URL:     https://github.com/thiagolopes/emacs-refined
;;; License: MIT

;;; Code:

;;; For performance
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024)) ;; 1mb
;;; (setq better-gc-cons-threshold 67108864) ; 64mb
(setq better-gc-cons-threshold 800000)
(add-hook 'emacs-startup-hook
	  (lambda ()
	    (if (boundp 'after-focus-change-function)
		(add-function :after after-focus-change-function
			      (lambda ()
				(unless (frame-focus-state)
				  (garbage-collect))))
	      (add-hook 'after-focus-change-function 'garbage-collect))
	    (defun gc-minibuffer-setup-hook ()
	      (setq gc-cons-threshold (* better-gc-cons-threshold 2)))
	    (defun gc-minibuffer-exit-hook ()
	      (garbage-collect)
	      (setq gc-cons-threshold better-gc-cons-threshold))
	    (add-hook 'minibuffer-setup-hook #'gc-minibuffer-setup-hook)
	    (add-hook 'minibuffer-exit-hook #'gc-minibuffer-exit-hook)))

;; disable package-enable
(setq package-enable-at-startup nil)

(let (file-name-handler-alist)
  ;; Ensure is running out of this file's directory
  (setq file-name-handler-alist nil)
  (setq user-emacs-directory (file-name-directory load-file-name)))

;;; Disable menu-bar, tool-bar, and scroll-bar.
(if (fboundp 'menu-bar-mode)
    (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode)
    (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode)
    (scroll-bar-mode -1))

;;; Setup package.el
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(unless package--initialized (package-initialize))

;;; Setup use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))
(setq use-package-always-ensure t)

;;; Useful Defaults
(setq-default cursor-type 'bar)	; Line-style cursor similar to other text editors
(setq inhibit-startup-screen t)	; Disable startup screen
(setq scroll-step 1 scroll-conservatively 10000)
(setq initial-scratch-message "Hi! emacs love you!")
(setq-default frame-title-format '("%b")) ; Make window title the buffer name
(setq ring-bell-function 'ignore)         ; Disable bell sound
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(fset 'yes-or-no-p 'y-or-n-p) ; y-or-n-p makes answering questions faster
(setq linum-format "%4d ")    ; Line number format
(visual-line-mode 1)	; enable visual line mode, "wrap lines in end"
(delete-selection-mode 1) ; Selected text will be overwritten when you start typing
(global-auto-revert-mode t) ; Auto-update buffer if file has changed on disk
(global-hl-line-mode 1)
(setq site-run-file nil)
(add-hook 'before-save-hook 'delete-trailing-whitespace) ; Delete trailing whitespace on save
(add-hook 'prog-mode-hook     ; Show line numbers in programming modes
	  (if (and (fboundp 'display-line-numbers-mode)
		   (display-graphic-p)) #'display-line-numbers-mode #'linum-mode))

;;; desktop-save-mode
(desktop-save-mode 1)
(setq desktop-restore-eager 4 desktop-save t)
(desktop-read)


;;; add recentf
(add-hook 'after-init-hook (recentf-mode 1))
(setq recentf-auto-cleanup "05:00am")
(setq recentf-max-saved-items 200)
(setq recentf-exclude '((expand-file-name package-user-dir) ".cache" ".cask" ".elfeed" "bookmarks"
			"cache" "ido.*" "persp-confs" "recentf" "undo-tree-hist" "url"
			"COMMIT_EDITMSG\\'"))
(save-place-mode 1)
(setq-default history-length 500)

;;; font confi
(defvar font-list '(("JetBrains Mono" . 10)
		    ("Input" . 10)
		    ("FiraCode" . 10)
		    ("Consolas" . 10)))
(defun switch-font ()
  "Documentation."
  (interactive)
  (let* (available-fonts font-name font-size font-setting)
    (dolist (font font-list
		  (setq available-fonts (nreverse available-fonts)))
      (when (member (car font)
		    (font-family-list))
	(push font available-fonts)))
    (if (not available-fonts)
	(message "No fonts from the chosen set are available")
      (if (called-interactively-p 'interactive)
	  (let* ((chosen (assoc-string (completing-read "What font to use? " available-fonts nil t)
				       available-fonts)))
	    (setq font-name (car chosen) font-size (read-number "Font size: " (cdr chosen))))
	(setq font-name (caar available-fonts) font-size (cdar available-fonts)))
      (setq font-setting (format "%s-%d" font-name font-size))
      (set-frame-font font-setting nil t)
      (add-to-list 'default-frame-alist (cons 'font font-setting)))))

(when (display-graphic-p)
  (switch-font))

;;; Keybindings
(global-set-key (kbd "C->") 'indent-rigidly-right-to-tab-stop) ; Indent selection by one tab length
(global-set-key (kbd "C-<") 'indent-rigidly-left-to-tab-stop) ; De-indent selection by one tab length
;;; Window navegation
(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)

;;; Ace window
(global-set-key (kbd "M-o") 'ace-window)
(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))

;;; Offload the custom-set-variables to a separate file
;;; This keeps your init.el neater and you have the option
;;; to gitignore your custom.el if you see fit.
(setq custom-file "~/.emacs.d/custom.el")
(unless (file-exists-p custom-file)
  (write-region "" nil custom-file))
;;; Load custom file. Don't hide errors. Hide success message
(load custom-file nil t)

;;; Load .emacs.d/lisp
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;;; Put Emacs auto-save and backup files to .emacs.d/tmp/
(setq temporary-file-directory (expand-file-name user-emacs-directory))
(defconst emacs-tmp-dir (expand-file-name (format "emacs%d" (user-uid)) temporary-file-directory))
(setq backup-by-copying t		; Avoid symlinks
      delete-old-versions t kept-new-versions 6 kept-old-versions 2 version-control t
      auto-save-list-file-prefix emacs-tmp-dir auto-save-file-name-transforms `((".*" ,emacs-tmp-dir
										 t)) ; Change autosave dir to tmp
      backup-directory-alist `((".*" . ,emacs-tmp-dir)))

;;; Lockfiles unfortunately cause more pain than benefit
(setq-default create-lockfiles nil)

;; Start server
(add-hook 'after-init-hook
	  (lambda ()
	    (require 'server)
	    (unless (server-running-p)
	      (server-start))))

;; better compilation
(setq-default compilation-always-kill t) ; kill compilation process before starting another
(setq-default compilation-ask-about-save nil) ; save all buffers on `compile'
(setq-default compilation-scroll-output t)

;; Add a newline automatically at the end of the file upon save.
(setq require-final-newline t)

;; Vertical Scroll
(setq scroll-step 1)
(setq scroll-margin 1)
(setq scroll-conservatively 101)
(setq scroll-up-aggressively 0.01)
(setq scroll-down-aggressively 0.01)
(setq auto-window-vscroll nil)
(setq fast-but-imprecise-scrolling nil)

;; Horizontal Scroll
(setq hscroll-step 1)
(setq hscroll-margin 1)

;; Load subword
(require 'subword)

;;; Load tools configs
(require 'init-helpers)
(require 'init-packages)
(require 'init-org)

;;; init ends here
