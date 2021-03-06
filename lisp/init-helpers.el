;;; init-helpers.el -*- lexical-binding: t; -*-

;;; Code:

(defun refined/change-state-on-view-mode ()
  (let ((is-active? view-mode)
	(bg-active (modus-themes-color 'red-refine-bg))
	(bg-inactive (modus-themes-color 'bg-active)))
    (setq-local cursor-type (if is-active? 'box 'bar))
    (face-remap-add-relative 'mode-line :background (if is-active? bg-active bg-inactive))))

;;; Automatically overriding stale locks
(defun emacs-process-p (pid)
  "If pid is the process ID of an emacs process, return t, else nil.
Also returns nil if pid is nil."
  (when pid (let* ((cmdline-file (concat "/proc/" (int-to-string pid) "/cmdline")))
	      (when (file-exists-p cmdline-file)
		(with-temp-buffer (insert-file-contents-literally cmdline-file)
				  (goto-char (point-min))
				  (search-forward "emacs" nil t) pid)))))

(defadvice desktop-owner (after pry-from-cold-dead-hands activate)
  "Don't allow dead emacsen to own the desktop file."
  (when (not (emacs-process-p ad-return-value))
    (setq ad-return-value nil)))


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


(defun refined/back-to-indentation-or-beginning ()
  (interactive)
  (if (= (point)
	 (progn (back-to-indentation)
		(point)))
      (beginning-of-line)))


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

;;; Binds to helpers
(global-set-key [M-down] 'refined/move-line-down)
(global-set-key [M-up] 'refined/move-line-up)
(global-set-key (kbd "C-<return>") 'refined/newline-at-end-of-line)
(global-set-key (kbd "C-a") 'refined/back-to-indentation-or-beginning)
(global-set-key [?\M- ] 'delete-horizontal-space)
(global-set-key (kbd "M-\\") 'just-one-space)

;; Truncate lines
(global-set-key (kbd "C-x C-l") #'toggle-truncate-lines)
;; Adjust font size like web browsers
(global-set-key (kbd "C-=") #'text-scale-increase)
(global-set-key (kbd "C-+") #'text-scale-increase)
(global-set-key (kbd "C--") #'text-scale-decrease)
;; Move up/down paragraph
(global-set-key (kbd "M-n") #'forward-paragraph)
(global-set-key (kbd "M-p") #'backward-paragraph)

(if (and (window-system)
	 (eq system-type 'gnu/linux))
    (refined/set-emacs-theme-dark))

(defalias 'switch-project 'projectile-switch-project)
(defalias 'find-projects-in 'projectile-discover-projects-in-directory)

;; hooks
(add-hook 'view-mode-hook 'refined/change-state-on-view-mode)

(provide 'init-helpers)
;;; init-helpers ends here
