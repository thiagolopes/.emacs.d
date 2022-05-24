;;; init-helpers.el -*- lexical-binding: t; -*-
;;; Commentary: this file has helpers to be used in project

;;; Code:
(defun refined/change-state-on-view-mode (color-active color-inactive)
  (face-remap-add-relative 'mode-line :background (if view-mode color-active color-inactive)))

(defun refined/toggle-fullscreen ()
  "Toggle full screen"
  (interactive)
  (set-frame-parameter nil 'fullscreen (when (not (frame-parameter nil 'fullscreen)) 'fullboth)))

(defun refined/reload-config ()
  (interactive)
  (load-file (concat user-emacs-directory "init.el")))

(defun refined/delete-this-file ()
  "Delete the current file, and kill the buffer."
  (interactive)
  (unless (buffer-file-name)
    (error
     "No file is currently being edited"))
  (when (yes-or-no-p (format "Really delete '%s'?" (file-name-nondirectory buffer-file-name)))
    (delete-file (buffer-file-name))
    (kill-this-buffer)))

(defun refined/rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
	(filename (buffer-file-name)))
    (unless filename
      (error
       "Buffer '%s' is not visiting a file!"
       name))
    (progn (when (file-exists-p filename)
	     (rename-file filename new-name 1))
	   (set-visited-file-name new-name)
	   (rename-buffer new-name))))

(defun refined/move-line-up ()
  (interactive)
  (transpose-lines 1)
  (forward-line -2)
  (indent-according-to-mode))

(defun refined/move-line-down ()
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1)
  (indent-according-to-mode))

(defun refined/newline-at-end-of-line ()
  (interactive)
  (move-end-of-line 1)
  (newline-and-indent))

(defun refined/kill-all-buffers ()
  (interactive)
  (mapc 'kill-buffer (delq (current-buffer)
			   (buffer-list))))

(defun refined/set-emacs-frames (variant)
  (dolist (frame (frame-list))
    (let* ((window-id (frame-parameter frame 'outer-window-id))
	   (id (string-to-number window-id))
	   (cmd (format "xprop -id 0x%x -f _GTK_THEME_VARIANT 8u -set _GTK_THEME_VARIANT \"%s\"" id
			variant)))
      (call-process-shell-command cmd))))

(defun refined/set-emacs-theme-dark ()
  (interactive)
  (refined/set-emacs-frames "dark"))

(defun switch-theme ()
  "An interactive funtion to switch themes."
  (interactive)
  (disable-theme (intern (car (mapcar #'symbol-name custom-enabled-themes))))
  (call-interactively #'load-theme))

(defun my-desktop-save ()
  (interactive)
  ;; Don't call desktop-save-in-desktop-dir, as it prints a message.
  (if (eq (desktop-owner)
	  (emacs-pid))
      (desktop-save desktop-dirname)))
(add-hook 'auto-save-hook 'my-desktop-save)

(defun workon ()
  (interactive)
  (venv-workon)
  (lsp-restart-workspace))

(defun hs-cycle (&optional level)
  (interactive "p")
  (let (message-log-max
	(inhibit-message t))
    (if (= level 1)
	(pcase last-command
	  ('hs-cycle
	   (hs-hide-level 1)
	   (setq this-command 'hs-cycle-children))
	  ('hs-cycle-children
	   ;; TODO: Fix this case. `hs-show-block' needs to be
	   ;; called twice to open all folds of the parent
	   ;; block.
	   (save-excursion (hs-show-block))
	   (hs-show-block)
	   (setq this-command 'hs-cycle-subtree))
	  ('hs-cycle-subtree
	   (hs-hide-block))
	  (_
	   (if (not (hs-already-hidden-p))
	       (hs-hide-block)
	     (hs-hide-level 1)
	     (setq this-command 'hs-cycle-children))))
      (hs-hide-level level)
      (setq this-command 'hs-hide-level))))

(defun refined/hs-global-cycle ()
    (interactive)
    (pcase last-command
      ('refined/hs-global-cycle
       (save-excursion (hs-show-all))
       (setq this-command 'hs-global-show))
      (_ (hs-hide-all))))

(defun refined/toggle-transparency ()
  (interactive)
  (let ((alpha (frame-parameter nil 'alpha)))
    (set-frame-parameter
     nil 'alpha
     (if (eql (cond ((numberp alpha) alpha)
		    ((numberp (cdr alpha)) (cdr alpha))
		    ;; Also handle undocumented (<active> <inactive>) form.
		    ((numberp (cadr alpha)) (cadr alpha)))
	      100)
	 '(98 . 95) '(100 . 100)))))

;;; Binds to helpers
(global-set-key [M-down] 'refined/move-line-down)
(global-set-key [M-up] 'refined/move-line-up)
(global-set-key (kbd "C-<return>") 'refined/newline-at-end-of-line)
(global-set-key [?\M- ] 'delete-horizontal-space)
(global-set-key (kbd "M-\\") 'just-one-space)
(global-set-key (kbd "C-<tab>") 'refined/hs-global-cycle)

;; Truncate lines
(global-set-key (kbd "C-x C-l") #'toggle-truncate-lines)
;; Adjust font size like web browsers
(global-set-key (kbd "C-=") #'text-scale-increase)
(global-set-key (kbd "C-+") #'text-scale-increase)
(global-set-key (kbd "C--") #'text-scale-decrease)
;; Move up/down paragraph
(global-set-key (kbd "M-n") #'forward-paragraph)
(global-set-key (kbd "M-p") #'backward-paragraph)
;; Use undo only
(global-set-key (kbd "C-/") 'undo-only)
(global-set-key (kbd "C-?") 'undo-redo)

(if (and (window-system)
	 (eq system-type 'gnu/linux))
    (refined/set-emacs-theme-dark))

(defalias 'switch-project 'projectile-switch-project)
(defalias 'find-projects-in 'projectile-discover-projects-in-directory)

;; hooks
(defun refined/change-state-on-view-mode-with-zenburn ()
  (let ((color-active (zenburn-with-color-variables zenburn-red-6))
	(color-inactive (zenburn-with-color-variables zenburn-bg-1)))
    (refined/change-state-on-view-mode color-active color-inactive)))

(add-hook 'view-mode-hook 'refined/change-state-on-view-mode-with-zenburn)

(provide 'init-helpers)
;;; init-helpers.el ends here
