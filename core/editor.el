;;; editor.el

;;
(require 'misc)
(forward-to-word 1)

;; but maintain correct appearance
(setq-default indent-tabs-mode nil)

;; delete the selection with a keypress
(delete-selection-mode t)

;; store all backup and autosave files in the tmp dir
(setq-default backup-directory-alist `(("." . ,(expand-file-name "backup" savefile-dir))))

;; revert buffers automatically when underlying files are changed externally
(global-auto-revert-mode t)

;; hippie expand is dabbrev expand on steroids
;; (setq-default hippie-expand-try-functions-list '(try-expand-dabbrev
;;                                          try-expand-dabbrev-all-buffers
;;                                          try-expand-dabbrev-from-kill
;;                                          try-complete-file-name-partially
;;                                          try-complete-file-name
;;                                          try-expand-all-abbrevs
;;                                          try-expand-list
;;                                          try-expand-line
;;                                          try-complete-lisp-symbol-partially
;;                                          try-complete-lisp-symbol))

;; smart tab behavior - indent or complete
(setq-default tab-always-indent 'complete)

;; disable boxes
(setq-default use-dialog-box nil)

;;; Useful Defaults
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
(setq uniquify-separator "/")
(setq uniquify-after-kill-buffer-p t)    ; rename after killing uniquified
(setq uniquify-ignore-buffers-re "^\\*") ; don't muck with special buffers

;;backup
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; https://www.emacswiki.org/emacs/SavePlace
;; saveplace remembers your location in a file when saving files
(setq-default save-place-file (expand-file-name "saveplace" savefile-dir))
;; activate it for all buffers
(save-place-mode 1)

;; savehist keeps track of some history
(require 'savehist)
(setq savehist-additional-variables
      ;; search entries
      '(search-ring regexp-search-ring)
      ;; save every minute
      savehist-autosave-interval 60
      ;; keep the home clean
      savehist-file (expand-file-name "savehist" savefile-dir))
(savehist-mode +1)

;; save recent files
(require 'recentf)
(setq recentf-save-file (expand-file-name "recentf" savefile-dir)
      recentf-max-saved-items 500
      recentf-max-menu-items 15
      ;; disable recentf-cleanup on Emacs start, because it can cause
      ;; problems with remote files
      recentf-auto-cleanup 'never)
(recentf-mode +1)

;; highlight the current line
;; (global-hl-line-mode +1)

;; linue number
;; (global-display-line-numbers-mode)
;; (setq-default display-line-numbers-width 3)

;; line spaccing
;; (setq-default line-spacing 0.3)

;; already disabled anyway
;; clean look
(blink-cursor-mode -1)
(scroll-bar-mode t)
(fringe-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(setq-default cursor-type 'bar)

;; disable the annoying bell ring
(setq-default ring-bell-function 'ignore)

;; disable startup screen
(setq inhibit-startup-screen t)

;; mode line settings
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

;; enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

;; more useful frame title, that show either a file or a
;; buffer name (if the buffer isn't visiting a file)
(setq frame-title-format
      '(""(:eval (if (buffer-file-name)
                     (abbreviate-file-name (buffer-file-name))
                   "%b"))))

;; dark theme gtk
(defun set-emacs-frames-gtk (variant)
  (dolist (frame (frame-list))
    (let* ((window-id (frame-parameter frame 'outer-window-id))
           (id (string-to-number window-id))
           (cmd (format "xprop -id 0x%x -f _GTK_THEME_VARIANT 8u -set _GTK_THEME_VARIANT \"%s\"" id
                        variant)))
      (call-process-shell-command cmd))))

(if (and (eq system-type 'gnu/linux) (display-graphic-p))
    (set-emacs-frames-gtk "dark"))

(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-=") #'text-scale-increase)
(global-set-key (kbd "C-+") #'text-scale-increase)
(global-set-key (kbd "C--") #'text-scale-decrease)
(global-set-key (kbd "M-n") #'forward-paragraph)
(global-set-key (kbd "M-p") #'backward-paragraph)
;; Use undo only
(global-set-key (kbd "C-/") 'undo-only)
(global-set-key (kbd "C-?") 'undo-redo)

;; Buffer resize
(global-set-key (kbd "M-<right>") (lambda ()
                                    (interactive)
                                    (shrink-window-horizontally 10)))
(global-set-key (kbd "M-<left>") (lambda ()
                                   (interactive)
                                   (enlarge-window-horizontally 10)))
(global-set-key (kbd "M-<down>") (lambda ()
                                   (interactive)
                                   (shrink-window 10)))
(global-set-key (kbd "M-<up>") (lambda ()
                                 (interactive)
                                 (enlarge-window 10)))

;; Buffer navegation
(global-set-key (kbd "<f3>") 'kmacro-start-macro-or-insert-counter)
(global-set-key (kbd "<f4>") 'kmacro-end-or-call-macro)

;; lisp sanitize
(global-set-key (kbd "M-3") '(lambda () (interactive) (insert "#")))
(global-set-key (kbd "M-9") '(lambda () (interactive) (insert "(")))
(global-set-key (kbd "M-0") '(lambda () (interactive) (insert ")")))

;; sanitize macos keys
(if (eq system-type 'darwin)
    (progn
      (setq mac-option-key-is-meta nil)
      (setq mac-command-key-is-meta t)
      (setq mac-command-modifier 'meta)
      (setq mac-option-modifier nil)))


;; modeline to top
(setq-default header-line-format mode-line-format)
(setq-default mode-line-format nil)

;; better scratch https://www.reddit.com/r/emacs/comments/4cmfwp/scratch_buffer_hacks_to_increase_its_utility/
(defun immortal-scratch ()
  (if (eq (current-buffer) (get-buffer "*scratch*"))
      (progn (bury-buffer)
             nil)
    t))
(add-hook 'kill-buffer-query-functions 'immortal-scratch)

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
(add-hook 'after-init-hook 'load-persistent-scratch)
(add-hook 'kill-emacs-hook 'save-persistent-scratch)
(run-with-idle-timer 300 t 'save-persistent-scratch)

;; initial window size
(defun set-frame-size-according-to-resolution ()
  (interactive)
  (if window-system
      (progn
        (if (> (x-display-pixel-width) 1280)
            (add-to-list 'default-frame-alist (cons 'width 120))
          (add-to-list 'default-frame-alist (cons 'width 80)))
        (add-to-list 'default-frame-alist
                     (cons 'height (x-display-pixel-height))))))
(set-frame-size-according-to-resolution)

;; avoid redraw until input process - test performance
(setq redisplay-dont-pause t)

(provide 'editor)
