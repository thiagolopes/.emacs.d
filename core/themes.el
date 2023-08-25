;;; load-theme
(require 'configs)

;; Minibuffer font increase
(defun my-minibuffer-setup ()
       (set (make-local-variable 'face-remapping-alist)
            '((default :height 1.1))))
(add-hook 'minibuffer-setup-hook 'my-minibuffer-setup)

(if (and (eq system-type 'gnu/linux) (display-graphic-p))
    (config/set-emacs-frames-gtk "dark"))

(setq-default indent-tabs-mode nil)
(setq-default line-spacing 0.15)

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

(use-package modus-themes
  :disabled
  :config
  (setq modus-themes-italic-constructs t
        modus-themes-bold-constructs t
        modus-themes-variable-pitch-ui t
        modus-themes-mixed-fonts t))
(use-package zenburn-theme)
(use-package minimal-theme)
(use-package jetbrains-darcula-theme)
(use-package gruvbox-theme)
(use-package gruber-darker-theme)
(use-package spacemacs-theme)
(use-package monokai-theme)
(use-package alect-themes)
(use-package dracula-theme)
(use-package blackboard-theme)
(use-package doom-themes)

(load-theme 'doom-gruvbox t)

(defalias 'change-theme 'consult-theme)

(provide 'themes)
