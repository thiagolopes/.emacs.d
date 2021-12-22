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
;; -- https://github.com/emacs-tw/awesome-emacs            ;;
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

;;; Disable menu-bar, tool-bar, and scroll-bar.
(if (fboundp 'menu-bar-mode)
    (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode)
    (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode)
    (scroll-bar-mode -1))

;;; Setup straight
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
(setq straight-use-package-by-default t)

;;; Add use-package
(straight-use-package 'use-package)

;;; Useful Defaults
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name)) "%b"))))
(setq inhibit-startup-screen t)	                         ; Disable startup screen
(setq scroll-step 1 scroll-conservatively 10000)
(setq initial-scratch-message ";;; Hi! emacs love you!")
(setq ring-bell-function 'ignore)                        ; Disable bell sound
(setq site-run-file nil)
(setq-default c-default-style "linux")
(setq-default c-tab-always-indent t)
(setq comment-style 'extra-line)
(setq-default display-fill-column-indicator-column 80)
(setq-default column-number-mode t)
(setq byte-compile-warnings '(cl-functions))
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(fset 'yes-or-no-p 'y-or-n-p)                            ; y-or-n-p makes answering questions faster
(visual-line-mode t)	                                 ; enable visual line mode, "wrap lines in end"
(delete-selection-mode t)                                ; Selected text will be overwritten when you start typing
(global-auto-revert-mode t)                              ; Auto-update buffer if file has changed on disk
(follow-mode 1)
(blink-cursor-mode 0)
(auto-composition-mode t)
(auto-image-file-mode t)
(fringe-mode '(4 . 0))

(add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)
(add-hook 'prog-mode-hook #'hs-minor-mode)

(setq-default display-line-numbers-width-start 5)
(setq-default display-line-numbers-width 5)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)

;;; desktop-save-mode
(desktop-save-mode 1)
(setq desktop-restore-eager 4 desktop-save t)

;;; font config
(defvar font-list '(("Hack" . 12)
		    ("JetBrains Mono" . 10)
		    ("Input" . 10)
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

;;; Window navegation
(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)

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

;;; Smoth scroll
(setq redisplay-dont-pause t
  scroll-margin 1
  scroll-step 1
  scroll-conservatively 101
  scroll-preserve-screen-position 1)
(setq mouse-wheel-progressive-speed nil)

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

;; better compilation
(setq-default compilation-always-kill t) ; kill compilation process before starting another
(setq-default compilation-ask-about-save nil) ; save all buffers on `compile'
(setq-default compilation-scroll-output t)

;; Add a newline automatically at the end of the file upon save.
(setq require-final-newline t)

;;; Load tools configs
(require 'init-helpers)
(require 'init-packages)
(require 'init-org)

;;; init ends here
