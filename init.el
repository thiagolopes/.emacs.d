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
;; https://emacs-lsp.github.io/lsp-mode/page/performance/
(setq gc-cons-threshold 100000000
      read-process-output-max (* 1024 1024))

;; https://www.masteringemacs.org/article/speed-up-emacs-libjansson-native-elisp-compilation
(if (and (fboundp 'native-comp-available-p)
         (native-comp-available-p))
    (setq comp-deferred-compilation t
          package-native-compile t)
  (message "Native complation is *not* available, lsp performance will suffer..."))

(unless (functionp 'json-serialize)
  (message "Native JSON is *not* available, lsp performance will suffer..."))

;; do not steal focus while doing async compilations
(setq warning-suppress-types '((comp)))

;; warn when opening files bigger than 100MB
(setq large-file-warning-threshold 100000000)

;;
(setq custom-file (concat user-emacs-directory "/custom.el"))
(add-to-list 'load-path (expand-file-name "core" user-emacs-directory))
(defvar savefile-dir (expand-file-name "savefile" user-emacs-directory)
  "This folder stores all the automatically generated save/history-files.")

(unless (file-exists-p savefile-dir)
  (make-directory savefile-dir))

;; avoid miss quit
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-z"))
(global-unset-key (kbd "M-m"))

;; load packages
(require 'better-defaults)
(require 'packages)
(require 'editor)
(require 'themes)

(when (display-graphic-p)
  (switch-font))

;; fix resize sluggish in xorg
(setq frame-resize-pixelwise t)

;;; init.el ends here
