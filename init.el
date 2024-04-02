;;; early-init.el -- only focus on emacs setup -*- lexical-binding: t; -*-
;;; Commentary:
;;;  to everthing early
;;; Code:
(when (version< emacs-version "29.0")
  (message "Your Emacs is old. Please upgrade if possible."))

(setq user-full-name "Thiago Lopes")
(setq user-mail-address "thiagolopes@protonmail.com")
(setq use-package-verbose t)
(setq use-package-compute-statistics nil)

;; Add padding inside frames (windows)
(add-to-list 'default-frame-alist '(internal-border-width . 2))

(set-window-buffer nil (current-buffer))

(add-hook 'before-save-hook 'whitespace-cleanup)

(context-menu-mode t)
(column-number-mode t)

 ;;; suggest a target for moving/copying intelligently
(setq dired-dwim-target t)
(setq dired-hide-details-hide-symlink-targets nil)
      ;; don't prompt to revert, just do it
(setq dired-auto-revert-buffer #'dired-buffer-stale-p)
      ;; Always copy/delete recursively
(setq dired-recursive-copies  'always)
(setq dired-recursive-deletes 'top)
      ;; Ask whether destination dirs should get created when copying/removing files.
(setq dired-create-destination-dirs 'ask)
      ;; Where to store image caches
(setq image-dired-dir (concat user-emacs-directory "image-dired/"))
(setq image-dired-db-file (concat image-dired-dir "db.el"))
(setq image-dired-gallery-dir (concat image-dired-dir "gallery/"))
(setq image-dired-temp-image-file (concat image-dired-dir "temp-image"))
(setq image-dired-temp-rotate-image-file (concat image-dired-dir "temp-rotate-image"))
      ;; Screens are larger nowadays, we can afford slightly larger thumbnails
(setq image-dired-thumb-size 150)

(setq site-run-file nil)
(setq inhibit-compacting-font-caches t)
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

(setq x-select-enable-clipboard-manager nil)
(setq warning-minimum-level :emergency)
(defalias 'yes-or-no-p 'y-or-n-p)
(show-paren-mode 1)

(setq show-paren-highlight-openparen nil)
(setq save-interprogram-paste-before-kill t)
(setq apropos-do-all t)
(setq mouse-yank-at-point t)
(setq require-final-newline t)
(setq frame-inhibit-implied-resize t)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; A simple frame title
(setq frame-title-format '("%b – Emacs"))
(setq icon-title-format frame-title-format)

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
(setq split-width-threshold 160)
(setq split-height-threshold nil)

;; FIX: The native border "consumes" a pixel of the fringe on righter-most
;;   splits, `window-divider' does not. Available since Emacs 25.1.
(setq window-divider-default-places t)
(setq window-divider-default-bottom-width 1)
(setq window-divider-default-right-width 1)
(add-hook 'prog-mode #'window-divider-mode)

;; Don't prompt for confirmation when we create a new file or buffer (assume the
;; user knows what they're doing).
(require 'uniquify)
(setq confirm-nonexistent-file-or-buffer nil)
(setq uniquify-buffer-name-style 'forward)

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

(setq visual-line-fringe-indicators t)

(electric-pair-mode t)

;; save position cursor
(require 'saveplace)
(setq-default save-place t)
(save-place-mode 1)
(setq save-place-file (concat user-emacs-directory "cache/places"))

;; setup dirs
(setq cache-dir (concat user-emacs-directory "/cache"))
(setq custom-file (concat user-emacs-directory "/custom.el"))
(setq require-final-newline t)
(setq load-prefer-newer t)

;; Made SHIFT+arrow to move to the next adjacent window in the specified direction
(windmove-default-keybindings)

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

(setq isearch-lax-whitespace t)
(setq isearch-regexp-lax-whitespace t)
(setq search-whitespace-regexp "[ \t\r\n]+")

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

;; Yes, I really want compile
(setq compilation-ask-about-save nil)

;; ctyle - change comentary to //
(add-hook 'c-mode-hook '(lambda () (setq comment-start "//"
                                         comment-end   "")))

(add-hook 'prog-mode #'electric-operator-mode)

;; copy-paste with ctrl c/v
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

;; (setq make-backup-files nil)
;; (setq backup-by-copying t)

;; backup setup
;;; don't clobber symlinks
(setq backup-by-copying t)
;;; don't litter my fs tree
(setq backup-directory-alist '(("." . "~/.emacs.d/var/saves/")))
(setq delete-old-versions t)
(setq kept-new-versions 6)
(setq kept-old-versions 2)
(setq version-control t)

(setq default-directory "~/")
;; overwrite text when selected, like we expect.
(setq delete-seleciton-mode t)
;; quiet startup
(setq inhibit-startup-message t)
;; hopefully all themes we install are safe
(setq custom-safe-themes t)
;; simple lock/backup file management
(setq create-lockfiles nil)
(setq delete-old-versions t)
;; when quiting emacs, just kill processes
(setq confirm-kill-processes nil)
;; ask if local variables are safe once.
(setq enable-local-variables t)
;; life is too short to type yes or no
(setq use-short-answers t)
;; clean up dired buffers
(setq dired-kill-when-opening-new-dired-buffer t)

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
(setq browse-url-browser-function 'browse-url-generic)

;; modeline which function
(which-function-mode -1)
(setq x-underline-at-descent-line t)

;; enable fringe mode
(fringe-mode)
(setq-default indicate-empty-lines t)
(setq-default indicate-buffer-boundaries 'left)

;; eletric-mode disabled
(electric-indent-mode -1)
(electric-pair-mode -1)

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

;; Virtual tall files
(follow-mode 1)
;; enable read only
(setq view-read-only t)
;; std flymake
(flymake-mode 1)

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

;; save only last buffer and window size
(setq desktop-load-locked-desktop t)
(setq desktop-files-not-to-save "^.*$")
(setq desktop-buffer-not-to-save "^.*$")
(desktop-save-mode 1)

;; org hooks
(add-hook 'org-mode-hook 'visual-line-mode)

;; set full file name on frame
(setq frame-title-format
      (list '(buffer-file-name "%f" "%b")
        '(:eval (format " - GNU Emacs %s" emacs-version))))



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



;; keys
;;; better navegate end file
(global-set-key (kbd "M-u") #'upcase-dwim)
(global-set-key (kbd "M-l") #'downcase-dwim)
(global-set-key (kbd "M-c") #'capitalize-dwim)
;;; mouse moviment
(global-set-key (kbd "<mouse-8>") #'previous-buffer)
(global-set-key (kbd "<mouse-9>") #'next-buffer)
;;; text-scale
(global-set-key (kbd "C-=") #'text-scale-increase)
(global-set-key (kbd "C-+") #'text-scale-increase)
(global-set-key (kbd "C--") #'text-scale-decrease)

;; add without shift
(global-set-key (kbd "M-3") '(lambda () (interactive) (insert "#")))
(global-set-key (kbd "M-9") '(lambda () (interactive) (insert "(")))
(global-set-key (kbd "M-0") '(lambda () (interactive) (insert ")")))
(global-set-key (kbd "M-[") '(lambda () (interactive) (insert "{")))
(global-set-key (kbd "M-]") '(lambda () (interactive) (insert "}")))

;; better buffer
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-c p") 'find-file-at-point)
(global-set-key (kbd "M-j") 'join-line)

;; eshell pop
(global-set-key (kbd "M-<f12>") '(lambda () (interactive) (term-pop 'eshell "*eshell*")))
(global-set-key (kbd "<f12>")   '(lambda () (interactive) (term-pop 'shell "*shell*")))

;; (global-set-key (kbd "M-n") #'forward-paragraph)
;; (global-set-key (kbd "M-p") #'backward-paragraph)
(global-set-key (kbd "<f10>") #'global-display-line-numbers-mode)
(global-set-key (kbd "C-,") #'duplicate-line)
(global-set-key (kbd "<f6>") #'font-lock-mode)


(load-file (expand-file-name "init-packages.el" user-emacs-directory))
;; (require 'init-packages)
(provide 'init)
;;; early-init.el ends here;
