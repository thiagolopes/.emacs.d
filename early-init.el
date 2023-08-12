;;; early-init.el --- Early Init File -*- lexical-binding: t -*-
;;; Code:

(defvar p-emacs-tiling-window-manager-regexp
  "\\(?:\\(?:bsp\\|herbstluft\\)wm\\)"
  "Regular expression to match desired tiling window managers.
See definition of `p-emacs-with-desktop-session'.")

(defmacro p-emacs-with-desktop-session (&rest body)
  "Expand BODY if desktop session is not a tiling window manager.
See `p-emacs-tiling-window-manager-regexp' for what
constitutes a matching tiling window manager."
  (declare (indent 0))
  `(when-let* ((session (getenv "DESKTOP_SESSION"))
               ((not (string-match-p session p-emacs-tiling-window-manager-regexp))))
     ,@body))

(defun p-emacs-add-to-list (list element)
  "Add to symbol of LIST the given ELEMENT.
Simplified version of `add-to-list'."
  (set list (cons element (symbol-value list))))

(p-emacs-with-desktop-session
  (mapc
   (lambda (var)
     (p-emacs-add-to-list var '(width . (text-pixels . 1400)))
     (p-emacs-add-to-list var '(height . (text-pixels . 900))))
   '(default-frame-alist initial-frame-alist)))

(setq frame-resize-pixelwise t
      frame-inhibit-implied-resize t
      use-dialog-box t ; only for mouse events, which I seldom use
      use-file-dialog nil
      inhibit-splash-screen t
      inhibit-startup-screen t
      inhibit-x-resources t
      inhibit-startup-echo-area-message user-login-name ; read the docstring
      inhibit-startup-buffer-menu t)

;; I do not use those graphical elements by default, but I do enable
;; them from time-to-time for testing purposes or to demonstrate
;; something.
(scroll-bar-mode t)
(fringe-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)

(setq file-name-handler-alist nil)
(setq vc-handled-backends nil)

;; Initialise installed packages at this early stage, by using the
;; available cache.  I had tried a setup with this set to nil in the
;; early-init.el, but (i) it ended up being slower and (ii) various
;; package commands, like `describe-package', did not have an index of
;; packages to work with, requiring a `package-refresh-contents'.
(setq package-enable-at-startup t)
