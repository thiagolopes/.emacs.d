;;; init.el -*- lexical-binding: t; -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs            					   ;;
;; -- Sanemacs version 0.3.0 as template		   ;;
;; -- spacemacs						   ;;
;; -- doomemacs						   ;;
;; -- purcell						   ;;
;; -- http://www.i3s.unice.fr/~malapert/emacs_orgmode.html ;;
;; -- https://github.com/manateelazycat/delete-block	   ;;
;; -- https://github.com/MatthewZMD/.emacs.d		   ;;
;; -- https://github.com/emacs-tw/awesome-emacs            ;;
;; Author:  Thiago Lopes <thiagolopes@pm.me>               ;;
;; URL:     https://github.com/thiagolopes/.emacs.d        ;;
;; License: MIT                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Code:
;;; For performance
(setq gc-cons-threshold 20000000)
(setq read-process-output-max (* 1024 1024)) ;; 1mb
(add-hook 'emacs-startup-hook
	  (lambda ()
	    (if (boundp 'after-focus-change-function)
		(add-function :after after-focus-change-function
			      (lambda ()
				(unless (frame-focus-state)
				  (garbage-collect))))
	      (add-hook 'after-focus-change-function 'garbage-collect))
	    (defun gc-minibuffer-setup-hook ()
	      (setq gc-cons-threshold gc-cons-threshold))
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
 (if (and (executable-find "watchexec")
          (executable-find "python3"))
     (setq straight-check-for-modifications '(watch-files find-when-checking))
   (setq straight-check-for-modifications
         '(find-at-startup find-when-checking)))
(setq straight-use-package-by-default t)

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


;;; Add use-package
(straight-use-package 'use-package)

;;; Useful Defaults
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name)) "%b"))))
(setq inhibit-startup-screen t)	                         ; Disable startup screen
(setq initial-scratch-message ";;; Hi! emacs love you!")
(setq ring-bell-function 'ignore)                        ; Disable bell sound
(setq site-run-file nil)
(setq-default c-default-style "linux")
(setq-default c-tab-always-indent t)
(setq comment-style 'extra-line)
(setq column-number-mode t)
(setq byte-compile-warnings '(cl-functions))
(setq make-pointer-invisible t)
(setq-default which-func-unknown "n/a")
(setq line-spacing 0.1)
(setq save-interprogram-paste-before-kill t)
(setq-default apropos-do-all t)
(setq require-final-newline t)
(setq backup-by-copying t)
(setq-default ediff-window-setup-function 'ediff-setup-windows-plain)
(setq scroll-margin 5)
(setq confirm-kill-processes nil)
(setq-default display-fill-column-indicator-column 120)
(setq-default cursor-type '(bar . 2))
(setq-default load-prefer-newer t)

(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(fset 'yes-or-no-p 'y-or-n-p)                            ; y-or-n-p makes answering questions faster
(visual-line-mode t)	                                 ; enable visual line mode, "wrap lines in end"
(delete-selection-mode t)                                ; Selected text will be overwritten when you start typing
(global-auto-revert-mode t)                              ; Auto-update buffer if file has changed on disk
(follow-mode t)
(blink-cursor-mode t)
(auto-composition-mode t)
(auto-image-file-mode t)
(fringe-mode '(4 . 2))
(which-function-mode t)

;;; desktop-save-mode
(desktop-save-mode 1)
(setq-default desktop-restore-eager 4 desktop-save t)

;;; font config
(defvar font-list '(
		    ("Hack" . 11)
		    ("Inconsolata" . 13)
		    ("Iosevka" . 13)
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

;; better compilation
(setq-default compilation-always-kill t) ; kill compilation process before starting another
(setq-default compilation-ask-about-save nil) ; save all buffers on `compile'
(setq-default compilation-scroll-output t)
(setq-default native-comp-async-report-warnings-errors nil)

;; Add a newline automatically at the end of the file upon save.
(setq require-final-newline t)

;;; Load tools configs
(require 'init-helpers)
(require 'init-org)
(require 'init-packages)

;;; init.el ends here
