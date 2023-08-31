;;; load-theme
(require 'configs)

;; modeline
(use-package spaceline
  :config
  (setq-default powerline-height 30)
  (spaceline-spacemacs-theme))
;; cursor
(setq-default cursor-type 'bar)

;;; font config
(defvar font-list '(
                    ("Fira Code" . 10)
                    ("Source Code Pro" . 12)
                    ("DejaVu Sans Mono" . 12)
                    ("Comic Mono" . 12)
                    ("Hack" . 12)
                    ("Victor Mono" . 12)
                    ("JetBrains Mono" . 14)
                    ("Inconsolata" . 14)
                    ("Iosevka" . 13)
                    ("Input" . 10)
                    ("Consolas" . 10)))
(defun switch-font ()
  "Documentation."
  (interactive)
  (let* (available-fonts font-name font-size font-setting)
    (dolist (font font-list
                  (setq available-fonts (nreverse available-fonts)))
      (when (member (car font)
                    (font-family-list))
        (push font available-fonts)))
    (if (not available-fonts)
        (message "No fonts from the chosen set are available")
      (if (called-interactively-p 'interactive)
          (let* ((chosen (assoc-string (completing-read "What font to use? " available-fonts nil t)
                                       available-fonts)))
            (setq font-name (car chosen) font-size (read-number "Font size: " (cdr chosen))))
        (setq font-name (caar available-fonts) font-size (cdar available-fonts)))
      (setq font-setting (format "%s-%d" font-name font-size))
      (set-frame-font font-setting nil t)
      (add-to-list 'default-frame-alist (cons 'font font-setting)))))

;; Minibuffer font increase
(defun my-minibuffer-setup ()
       (set (make-local-variable 'face-remapping-alist)
            '((default :height 1.2))))
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
