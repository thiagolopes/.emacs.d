;;; editor.el

;; Delete the selection with a keypress
(delete-selection-mode t)

;; Store all backup and autosave files in the tmp dir
(setq-default backup-directory-alist `(("." . ,(expand-file-name "backup" savefile-dir))))

;; Revert buffers automatically when underlying files are changed externally
(global-auto-revert-mode t)

;;; Make names uniques
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
(setq uniquify-separator "/")
(setq uniquify-after-kill-buffer-p t)    ; rename after killing uniquified
(setq uniquify-ignore-buffers-re "^\\*") ; don't muck with special buffers

;; https://www.emacswiki.org/emacs/SavePlace
;; Saveplace remembers your location in a file when saving files and activate it for all buffers
(setq-default save-place-file (expand-file-name "saveplace" savefile-dir))
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

;; more useful frame title, that show either a file or a
;; buffer name (if the buffer isn't visiting a file)
;; (setq frame-title-format
;;       '(""(:eval (if (buffer-file-name)
;;                      (abbreviate-file-name (buffer-file-name))
;;                    "%b"))))

(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

(global-set-key (kbd "C-=") #'text-scale-increase)
(global-set-key (kbd "C-+") #'text-scale-increase)
(global-set-key (kbd "C--") #'text-scale-decrease)
(global-set-key (kbd "M-n") #'forward-paragraph)
(global-set-key (kbd "M-p") #'backward-paragraph)

;; Buffer navegation
(global-set-key (kbd "<f3>") 'kmacro-start-macro-or-insert-counter)
(global-set-key (kbd "<f4>") 'kmacro-end-or-call-macro)

;; lisp sanitize
(global-set-key (kbd "M-3") '(lambda () (interactive) (insert "#")))
(global-set-key (kbd "M-9") '(lambda () (interactive) (insert "(")))
(global-set-key (kbd "M-0") '(lambda () (interactive) (insert ")")))
(global-set-key (kbd "M-[") '(lambda () (interactive) (insert "{")))
(global-set-key (kbd "M-]") '(lambda () (interactive) (insert "}")))

;; sanitize macos keys
(if (eq system-type 'darwin)
    (progn
      (setq mac-option-key-is-meta nil)
      (setq mac-command-key-is-meta t)
      (setq mac-command-modifier 'meta)
      (setq mac-option-modifier nil)))

;; better scratch https://www.reddit.com/r/emacs/comments/4cmfwp/scratch_buffer_hacks_to_increase_its_utility/
(defun immortal-scratch ()
  (if (eq (current-buffer) (get-buffer "*scratch*"))
      (progn (bury-buffer)
             nil) t))
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

;; focus at compilation buffer
(defadvice compile (after jump-back activate)
  (other-window 1))

;; Fix compilation buffer asci caracheters
(require 'ansi-color)
(defun ansi-colorize-buffer ()
  (let ((buffer-read-only nil))
    (ansi-color-apply-on-region (point-min) (point-max))))
(add-hook 'compilation-filter-hook 'ansi-colorize-buffer)

;; https://emacs.stackexchange.com/questions/55912/temporarily-show-the-documentation-for-a-emacs-lisp-function
(define-advice elisp-get-fnsym-args-string (:around (orig-fun sym &rest r) docstring)
  "If SYM is a function, append its docstring."
  (concat
   (apply orig-fun sym r)
   (let* ((doc (and (fboundp sym) (documentation sym 'raw)))
          (oneline (and doc (substring doc 0 (string-match "\n" doc)))))
     (and oneline
          (stringp oneline)
          (not (string= "" oneline))
          (concat "  |  " (propertize oneline 'face 'italic))))))

;; dired with larger font
(global-set-key (kbd "C-x C-d") 'dired)

;;
(global-set-key "\M- " 'hippie-expand)
(setq-default try-expand-dabbrev-visible t)
(setq-default try-expand-dabbrev t)
(setq-default try-expand-dabbrev-all-buffers t)

(provide 'editor)
