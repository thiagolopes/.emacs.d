;;; init-helpers.el -*- lexical-binding: t; -*-

;;; Code:

(defun refined/reload-config ()
  (interactive)
  (load-file (concat user-emacs-directory "init.el")))


(defun refined/delete-this-file ()
  "Delete the current file, and kill the buffer."
  (interactive)
  (unless (buffer-file-name)
    (error "No file is currently being edited"))
  (when (yes-or-no-p (format "Really delete '%s'?"
                             (file-name-nondirectory buffer-file-name)))
    (delete-file (buffer-file-name))
    (kill-this-buffer)))


(defun refined/rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (unless filename
      (error "Buffer '%s' is not visiting a file!" name))
    (progn
      (when (file-exists-p filename)
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
  (if (= (point) (progn (back-to-indentation) (point)))
      (beginning-of-line)))

;;; Binds to helpers
(global-set-key [M-down] 'refined/move-line-down)
(global-set-key [M-up] 'refined/move-line-up)
(global-set-key (kbd "C-<return>") 'refined/newline-at-end-of-line)
(global-set-key (kbd "C-a") 'refined/back-to-indentation-or-beginning)

(provide 'init-helpers)
;;; init-helpers ends here
