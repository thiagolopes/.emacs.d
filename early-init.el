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
;; style add padding inside frames (windows)
(add-to-list 'default-frame-alist '(internal-border-width . 2))

;; remove ui
(tool-bar-mode 0)
(scroll-bar-mode 0)
(horizontal-scroll-bar-mode 0)
(menu-bar-mode 0)

(provide 'early-init)
