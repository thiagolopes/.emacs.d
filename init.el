;;; early-init.el -- only focus on emacs setup -*- lexical-binding: t; -*-
;;; Commentary:
;;;  to everthing early
;;; Code:
(when (version< emacs-version "29.0")
  (message "Your Emacs is old. Please upgrade if possible."))

;;by
(setq user-full-name "Thiago Lopes")
(setq user-mail-address "thiagolopes@protonmail.com")
;; sanitaze
;; (setq x-select-enable-clipboard-manager nil)
(setq require-final-newline t)
(defalias 'yes-or-no-p 'y-or-n-p)
(setq use-short-answers t)

;; disable multiple eldoc
;; (setq eldoc-echo-area-use-multiline-p nill)

;; save cursor position
(require 'saveplace)
(setq save-place t)
(save-place-mode t)

;; change default width
(setq-default fill-column 100)

;; smart parens
(electric-pair-mode 1)

;; right click mouse
(context-menu-mode t)

;; show column number at modeline
(column-number-mode t)

;; DIRED
;; dired always copy/delete recursively
(setq dired-recursive-copies 'always)
(setq dired-listing-switches "-alh")

;; recursive, allow M-x inside minibuffer
(setq enable-recursive-minibuffers nil)

;; never use dialog box
(setq use-dialog-box nil)

;; middle-click paste at point, not at click
(setq mouse-yank-at-point t)

;; cursor config
(blink-cursor-mode 0)

;; disable /etc/emacs/site-start to run
(setq site-run-file nil)

;; vertical candites
;; (icomplete-mode t)
;; (define-key icomplete-minibuffer-map (kbd "TAB") #'icomplete-force-complete)
;; (icomplete-vertical-mode t)

;; ;; avoid cursor recenter every scroll
;; (setq scroll-margin 3)
(setq scroll-conservatively 101)
(setq scroll-up-aggressively 0.01)
(setq scroll-down-aggressively 0.01)
;; (setq scroll-preserve-screen-position t)
;; (setq auto-window-vscroll nil)
;; (setq text-scale-mode-step 1.05)

;; (setq tab-always-indent t)

;; Explicitly define a width to reduce the cost of on-the-fly computation
;; (setq-default display-line-numbers-width 3)
;; (setq-default display-line-numbers-widen t)
;; (add-hook 'prog-mode-hook #'display-line-number-mode)
;; (add-hook 'text-mode-hook #'display-line-number-mode)
;; (add-hook 'conf-mode-hook #'display-line-number-mode)
;; (setopt display-line-numbers-type t) ;; disable line number as default

;; Line number scale equal
(add-hook 'text-scale-mode-hook (lambda() (face-remap--remap-face 'line-number)))
;; (add-hook 'text-scale-mode-hook (lambda() (face-remap--remap-face 'line-number-current-line)))

;; ;; saveaition cursor
;; (save-place-mode 1)
;; (setopt save-place-file (concat user-emacs-directory "cache/places"))

;; setup dirs
(setq custom-file (concat user-emacs-directory "/custom.el"))

;; reload file if change
(setq load-prefer-newer t)
(setq auto-revert-avoid-polling t)
(setq auto-revert-interval 5)
(setq auto-revert-check-vc-info t)
(global-auto-revert-mode)

;; Made SHIFT+arrow to move to the next adjacent window in the specified direction
(windmove-default-keybindings)

;; ;; TODO
;; (setopt isearch-lax-whitespace t)
;; (setopt isearch-regexp-lax-whitespace t)
;; (setopt search-whitespace-regexp "[ \t\r\n]+")

;; ;; Yes, I really want to quit.
;; (setopt confirm-kill-emacs nil)

;; ;; Yes, I really want compile
;; (setopt compilation-ask-about-save nil)

;; ctyle - change comentary to /
(setq-default c-basic-offset 4
	      c-default-style '((java-mode . "java")
				(awk-mode . "awk")
				(other . "bsd")))

(add-hook 'c-mode-hook (lambda ()
			 (interactive)
			 (c-toggle-comment-style -1)))

(add-hook 'c-mode-hook (lambda () (setq comment-start "//"
					comment-end   "")))
;; ;; follow mode to split 1 file in to buffers
;; (follow-mode 1)

;; ;; copy-paste with ctrl c/v
;; (cua-mode 0)

;; looks horrible
;; treat camel-cased words as individual words.
;; (add-hook 'prog-mode-hook 'subword-mode)

;; don't assume sentences end with two spaces after a period.
;; (setopt sentence-end-double-space nil)

;; handle very long lines without hurting emacs
(global-so-long-mode)

;; backup setup
;; don't clobber symlinks
(setq backup-by-copying t)
(setq backup-directory-alist '(("." . "~/.emacs.d/var/saves/")))
(setq delete-old-versions t)
(setq kept-new-versions 6)
(setq kept-old-versions 2)
(setq version-control t)

;; overwrite text when selected, like we expect.
;; (setq delete-seleciton-mode t)

;; when quiting emacs, just kill processes
(setq confirm-kill-processes nil)
(setq confirm-kill-emacs 'y-or-n-p)

;; ;; show dir first in dired
(require 'ls-lisp)
(setq ls-lisp-dirs-first t)
(setq ls-lisp-use-insert-directory-program nil)

;; utf8, please
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(set-file-name-coding-system 'utf-8)
(set-clipboard-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)

;; default browser
(setq browse-url-browser-function 'browse-url-default-browser)

;; better underline
(setq x-underline-at-descent-line t)

;; show parens
(show-paren-mode t)

;; enable fringe mode
(fringe-mode)
(setq-default indicate-empty-lines t)
(setq-default indicate-buffer-boundaries 'left)
(setq visual-line-fringe-indicators '(nil right-curly-arrow))

;; Hippie-expand
;; (hippie-expand t)
;; (setopt hippie-expand-try-functions-list
;;       '(try-expand-dabbrev
;;	try-expand-dabbrev-all-buffers
;;	;; try-expand-dabbrev-from-kill
;;	;; try-expand-all-abbrevs
;;	;; try-expand-list
;;	;; try-expand-lien
;;	try-complete-lisp-symbol-partially
;;	try-complete-lisp-symbol
;;	try-complete-file-name-partially
;;	try-complete-file-name))
;; (global-set-key [remap dabbrev-expand] 'hippie-expand)

;; save m-x history
(setq history-length t)
(setq history-delete-duplicates t)
(setq savehist-save-minibuffer-history t)
(savehist-mode t)

;; org-mode
(setq org-directory "~/org/")
(setq org-todo-keywords
      '((sequence "TODO" "VERIFY" "|" "DONE" "STOPED")))

;; python use async in run-python
(setq python-shell-interpreter-args "-i -m asyncio")

;; save desktop only window size, without questions - stolen from Xat
;; (defun desktop-file-modtime-reset ()
;;   "Reset `desktop-file-modtime' so the user is not bothered."
;;   (interactive)
;;   (run-with-timer 5 nil
;;	  (lambda ()
;;	    (setq desktop-file-modtime (nth 5 (file-attributes (desktop-full-file-name))))
;;	    (desktop-save user-emacs-directory))))
;; (defun desktop-settings-setup ()
;;   "Some settings setup for desktop-save-mode."
;;   (interactive)
;;   ;; do not save any buffer
;;   (setopt desktop-files-not-to-save "^.*$")
;;   (setopt desktop-buffer-not-to-save "^.*$")
;;   ;; Here we activate the desktop mode
;;   (desktop-save-mode 1)
;;   ;; The default desktop is saved always
;;   (setq desktop-save t)
;;   ;; The default desktop is loaded anyway if it is locked
;;   (setq desktop-load-locked-desktop t)
;;   ;; Set the location to save/load default desktop
;;   (setq desktop-dirname user-emacs-directory)
;;   ;; Make sure that even if emacs or OS crashed, emacs still have last opened files.
;;   (add-hook 'find-file-hook 'desktop-file-modtime-reset)
;;   ;; Read default desktop
;;   (if (file-exists-p (concat desktop-dirname desktop-base-file-name))
;;       (desktop-read desktop-dirname))
;;   ;; Add a hook when emacs is closed to we reset the desktop modification time
;;   ;;(in this way the user does not get a warning message about desktop modifications)
;;   ;; (add-hook 'kill-emacs-hook 'desktop-file-modtime-reset))
;;   )
;; (add-hook 'after-init-hook 'desktop-settings-setup "APPEND")

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

;; (setq mode-line-format
;;       '("%e" mode-line-front-space
;;         (:propertize
;;          ("" mode-line-mule-info mode-line-client mode-line-modified
;;           mode-line-remote)
;;          display (min-width (1.0)))
;;         mode-line-frame-identification
;;         mode-line-buffer-identification
;;         " - "
;;         mode-line-
;;         minions-mode-line-modes
;;         mode-line-misc-info
;;         (vc-mode vc-mode) "  "
;;         mode-line-end-spaces))

;; set full file name on frame
(setq frame-title-format
      (list '(buffer-file-name "%f" "%b")
	'(:eval (format " - GNU Emacs %s" emacs-version))))
(setq icon-title-format frame-title-format)

;; Completion preview
;; (load (expand-file-name "completion-preview.el" user-emacs-directory))
;; (add-hook 'prog-mode-hook #'completion-preview-mode)
;; (add-hook 'text-mode-hook #'completion-preview-mode)
;; (require 'completion-preview)


;; modeline which function
;; (add-hook 'prog-mode-hook which-function-mode)
;; ;; line wrapper by word
(add-hook 'text-mode-hook #'visual-line-mode)
;; ;; line number enable
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
;;; remove white spaces
(add-hook 'before-save-hook #'whitespace-cleanup)


;; eshell pop
;; (defun term-pop (fn name)
;;   (if (not (get-buffer-window name))
;;     (progn
;;       (split-window-below 10)
;;       ;; (other-window -1)
;;       (switch-to-buffer (next-buffer))
;;       (funcall fn))
;;     (if (equal (buffer-name) name)
;;     (delete-window)
;;     (switch-to-buffer-other-window name))))


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
(setq initial-major-mode 'fundamental-mode) ;; be org-mode


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



(defun ansi-colors-enable ()
  (interactive)
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region (point-min) (point-max))))


;; Setup dark GTK theme if available only on X11
(defun set-emacs-frames-gtk (variant)
  (dolist (frame (frame-list))
    (let* ((window-id (frame-parameter frame 'outer-window-id))
       (id (string-to-number window-id))
       (cmd (format "xprop -id 0x%x -f _GTK_THEME_VARIANT 8u -set _GTK_THEME_VARIANT \"%s\"" id
	    variant)))
      (call-process-shell-command cmd))))
(when (and (eq system-type 'gnu/linux) (display-graphic-p))
  (set-emacs-frames-gtk "dark"))


;; uncessary
(global-unset-key (kbd "M-<down-mouse-1>"))
;;; better navegate end file
(global-set-key (kbd "M-u") #'upcase-dwim)
(global-set-key (kbd "M-l") #'downcase-dwim)
(global-set-key (kbd "M-c") #'capitalize-dwim)
;; ;;; mouse moviment
(global-set-key (kbd "<mouse-8>") #'previous-buffer)
(global-set-key (kbd "<mouse-9>") #'next-buffer)
;;; text-scale
(global-set-key (kbd "C-=") #'text-scale-increase)
(global-set-key (kbd "C-+") #'text-scale-increase)
(global-set-key (kbd "C--") #'text-scale-decrease)

;; ;; add without shift
;; (global-set-key (kbd "M-3") #'(lambda () (interactive) (insert "#")))
;; (global-set-key (kbd "M-9") #'(lambda () (interactive) (insert "(")))
;; (global-set-key (kbd "M-0") #'(lambda () (interactive) (insert ")")))
;; (global-set-key (kbd "M-[") #'(lambda () (interactive) (insert "{")))
;; (global-set-key (kbd "M-]") #'(lambda () (interactive) (insert "}")))

;; better buffer
(global-set-key (kbd "C-x C-b") #'ibuffer)
(global-set-key (kbd "C-c p") #'find-file-at-point)
(global-set-key (kbd "M-j") #'join-line)
;;dired
(global-set-key (kbd "C-x C-d") #'dired)

;; eshell pop
;; (global-set-key (kbd "M-<f12>") #'(lambda () (interactive) (term-pop 'eshell "*eshell*")))
;; (global-set-key (kbd "<f12>")   '(lambda () (interactive) (term-pop 'shell "*shell*")))

(global-set-key (kbd "<f10>") #'display-line-numbers-mode)
(global-set-key (kbd "C-,") #'duplicate-line)
(global-set-key (kbd "<f6>") #'font-lock-mode)

(when (display-graphic-p)
      (load (expand-file-name "init-packages.el" user-emacs-directory)))

(provide 'init)
;;; early-init.el ends here;
