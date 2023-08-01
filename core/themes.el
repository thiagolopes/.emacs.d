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
(use-package gruvbox-theme)

(load-theme 'zenburn t)

(defalias 'change-theme 'consult-theme)

(provide 'themes)
