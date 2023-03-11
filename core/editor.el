;; don't use tabs to indent
;; but maintain correct appearance
(setq-default indent-tabs-mode nil)
(setq-default tab-width 8)
;; delete the selection with a keypress
(delete-selection-mode t)

;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist `(("." . ,(expand-file-name "backup" savefile-dir))))

;; revert buffers automatically when underlying files are changed externally
(global-auto-revert-mode t)

;; hippie expand is dabbrev expand on steroids
(setq hippie-expand-try-functions-list '(try-expand-dabbrev
                                         try-expand-dabbrev-all-buffers
                                         try-expand-dabbrev-from-kill
                                         try-complete-file-name-partially
                                         try-complete-file-name
                                         try-expand-all-abbrevs
                                         try-expand-list
                                         try-expand-line
                                         try-complete-lisp-symbol-partially
                                         try-complete-lisp-symbol))

;; smart tab behavior - indent or complete
(setq tab-always-indent 'complete)

;;; Useful Defaults
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
(setq uniquify-separator "/")
(setq uniquify-after-kill-buffer-p t)    ; rename after killing uniquified
(setq uniquify-ignore-buffers-re "^\\*") ; don't muck with special buffers

;; https://www.emacswiki.org/emacs/SavePlace
;; saveplace remembers your location in a file when saving files
(setq save-place-file (expand-file-name "saveplace" savefile-dir))
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
(global-hl-line-mode +1)

(provide 'editor)
