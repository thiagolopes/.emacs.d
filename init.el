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
(setq gc-cons-threshold-original gc-cons-threshold)
(setq gc-cons-threshold (* 1024 1024 1024))
(setq read-process-output-max (* 1024 1024)) ;; 1mb

(add-hook 'after-init-hook #'(lambda ()
			       ;; restore after startup
			       (setq gc-cons-threshold gc-cons-threshold-original)))

(let (file-name-handler-alist)
  ;; Ensure is running out of this file's directory
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
(setq-default cursor-type 'bar)           ; Line-style cursor similar to other text editors
(setq inhibit-startup-screen t)           ; Disable startup screen
(setq scroll-step 1
      scroll-conservatively 10000)
(setq initial-scratch-message "Hi! emacs love you!")
(setq-default frame-title-format '("%b")) ; Make window title the buffer name
(setq ring-bell-function 'ignore)         ; Disable bell sound
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(fset 'yes-or-no-p 'y-or-n-p)             ; y-or-n-p makes answering questions faster
(setq linum-format "%4d ")                ; Line number format
(desktop-save-mode 1)
(setq desktop-load-locked-desktop nil)
(visual-line-mode 1)                      ; enable visual line mode, "wrap lines in end"
(setq desktop-restore-eager 4)
(delete-selection-mode 1)                 ; Selected text will be overwritten when you start typing
(global-auto-revert-mode t)               ; Auto-update buffer if file has changed on disk
(add-hook 'before-save-hook
	  'delete-trailing-whitespace)    ; Delete trailing whitespace on save
(add-hook 'prog-mode-hook                 ; Show line numbers in programming modes
	  (if (and (fboundp 'display-line-numbers-mode) (display-graphic-p))
	      #'display-line-numbers-mode
	    #'linum-mode))

;;; font config
(set-face-attribute 'default nil
                    :family "JetBrains Mono"
                    :height 100
                    :weight 'normal
                    :width 'normal)

;;; Keybindings
(global-set-key (kbd "C->") 'indent-rigidly-right-to-tab-stop) ; Indent selection by one tab length
(global-set-key (kbd "C-<") 'indent-rigidly-left-to-tab-stop)  ; De-indent selection by one tab length
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
(setq
   backup-by-copying t                                        ; Avoid symlinks
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   version-control t
   auto-save-list-file-prefix emacs-tmp-dir
   auto-save-file-name-transforms `((".*" ,emacs-tmp-dir t))  ; Change autosave dir to tmp
   backup-directory-alist `((".*" . ,emacs-tmp-dir)))

;;; Lockfiles unfortunately cause more pain than benefit
(setq create-lockfiles nil)

;; Start server
(add-hook 'after-init-hook
	  (lambda ()
	    (require 'server)
	    (unless (server-running-p)
	      (server-start))))

;;; Load tools configs
(require 'init-helpers)
(require 'init-packages)
(require 'init-org)

;;; init ends here
