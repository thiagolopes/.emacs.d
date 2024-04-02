;; GC conf
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024))
(setq comp-deferred-compilation t)
(defvar comp-deferred-compliation)
(add-hook 'after-init-hook #'garbage-collect t)

;; load theme
(setq greenized-theme-default-text-color t)
(setq greenized-theme-mixed-fonts t)
(load-theme 'greenized t)

;; remove ui
(tool-bar-mode -1)
(scroll-bar-mode -1)
(horizontal-scroll-bar-mode -1)
(menu-bar-mode -1)

(provide 'early-init)
