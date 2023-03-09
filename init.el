;;; init.el -*- lexical-binding: t; -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs            					   ;;
;; Author:  Thiago Lopes <thiagolopes@pm.me>               ;;
;; URL:     https://github.com/thiagolopes/.emacs.d        ;;
;; License: MIT                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Custom
(setq custom-file "~/.emacs.d/custom.el")
(unless (file-exists-p custom-file)
  (write-region "" nil custom-file))
(load custom-file nil t)

;;; GUI
(custom-set-variables
 '(inhibit-startup-screen t)
 '(menu-bar-mode nil)
 '(tool-bar-mode nil)
 '(scroll-bar-mode nil)
 '(use-dialog-box nil)
 '(ring-bell-function #'ignore)
 '(tab-bar-show 1))

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

(custom-set-variables
 '(cursor-type '(bar . 2))
 '(line-spacing 0.22)
 '(global-auto-revert-mode t)
 '(auto-revert-interval 2)
 '(auto-revert-check-vc-info t)
 '(global-auto-revert-non-file-buffers t)
 '(auto-revert-verbose nil)
 '(save-interprogram-paste-before-kill t)
 '(apropos-do-all t)
 '(load-prefer-newer t)
 '(savehist-mode 1)
 '(show-paren-mode 1)
 '(save-place-mode 1)
 '(uniquify-buffer-name-style 'forward)
 '(scroll-step 1)
 '(set-mark-command-repeat-pop t)
 '(tab-always-indent 'complete)
 '(current-language-environment "UTF-8")
 '(after-save-hook '(executable-make-buffer-file-executable-if-script-p))
 '(column-number-indicator-zero-based nil)
 '(scroll-preserve-screen-position t)
 '(make-backup-files nil)
 '(sentence-end-double-space nil)
 '(words-include-escapes t)
 '(indent-tabs-mode nil)
 '(standard-indent 2)
 '(view-read-only t)
 '(kill-read-only-ok t)
 '(kill-whole-line t)
 '(history-delete-duplicates t)
 '(kill-do-not-save-duplicates t)
 '(password-cache-expiry 300)
 '(debugger-stack-frame-as-list t)
 '(split-width-threshold 140)
 '(y-or-n-p-use-read-key t)
 '(use-short-answers t)
 '(async-shell-command-display-buffer nil)
 '(revert-without-query '(""))
 '(recenter-positions '(top middle bottom))
 '(display-time-default-load-average nil)
 '(dictionary-server "dict.org")
 '(set-window-margin nil 2)
 '(frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name)) "%b")))))

(global-unset-key (kbd "C-z")) ; avoid miss quit

;; save deskto - save and restore session
(setq desktop-dirname             "~/.emacs.d/desktop/"
      desktop-base-file-name      "emacs.desktop"
      desktop-base-lock-name      "lock"
      desktop-save                t
      desktop-auto-save-timeout   30)
 (desktop-save-mode 1)

;;; font config
(defvar font-list '(
                    ("Victor Mono" . 12)
                    ("Source Code Pro" . 13)
		    ("JetBrains Mono" . 14)
		    ("Inconsolata" . 15)
		    ("Hack" . 11)
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
(when (display-graphic-p)
  (switch-font))

;;; Load .emacs.d/lisp
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
;;; Load tools configs
(require 'init-helpers)
(require 'init-org)
(require 'init-packages)
(require 'init-modeline)
;;; init.el ends here
