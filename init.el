;;; init.el -*- lexical-binding: t; -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs            					   ;;
;; Author:  Thiago Lopes <thiagolopes@pm.me>               ;;
;; URL:     https://github.com/thiagolopes/.emacs.d        ;;
;; License: MIT                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Define and initialise package repositories
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; use-package to simplify the config file
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure 't)

;; Always load newest byte code
(setq load-prefer-newer t)

;; Always
(setq require-final-newline t)

;; reduce the frequency of garbage collection by making it happen on
;; each 50MB of allocated data (the default is on every 0.76MB)
(setq gc-cons-threshold 50000000)

;; warn when opening files bigger than 100MB
(setq large-file-warning-threshold 100000000)

;;; font config
(defvar font-list '(
                    ("Comic Mono" . 12)
                    ("DejaVu Sans Mono" . 12)
                    ("Hack" . 12)
                    ("Victor Mono" . 12)
                    ("Source Code Pro" . 13)
	 	    ("JetBrains Mono" . 14)
		    ("Inconsolata" . 15)
		    ("Iosevka" . 13)
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


;;
(setq custom-file (concat user-emacs-directory "/custom.el"))
(add-to-list 'load-path (expand-file-name "core" user-emacs-directory))
(defvar savefile-dir (expand-file-name "savefile" user-emacs-directory)
  "This folder stores all the automatically generated save/history-files.")

(unless (file-exists-p savefile-dir)
  (make-directory savefile-dir))

;; avoid miss quit
(global-unset-key (kbd "C-z"))

;; load packages
(require 'editor)
(require 'packages)

;;; load-theme
(use-package gruber-darker-theme
  :init
  (load-theme 'gruber-darker t))

(when (display-graphic-p)
  (switch-font))

;;; init.el ends here
