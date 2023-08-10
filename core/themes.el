;;; load-theme
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

(load-theme 'blackboard t)

(defalias 'change-theme 'consult-theme)

(provide 'themes)
