;; GC conf
(setopt gc-cons-threshold 100000000)
(setopt read-process-output-max (* 1024 1024))
(setopt comp-deferred-compilation t)
(defvar comp-deferred-compliation)
(add-hook 'after-init-hook #'garbage-collect t)

;; please, maximize
(add-to-list 'default-frame-alist '(fullscreen . maximized))
;; quiet startup
(setq inhibit-startup-message t)
;; hopefully all themes we install are safe
(setq custom-safe-themes t)
;; load theme
(load-theme 'greenized t)
;; style add padding inside frames (windows)
(add-to-list 'default-frame-alist '(internal-border-width . 2))

;; remove ui
(tool-bar-mode 0)
(scroll-bar-mode 0)
(horizontal-scroll-bar-mode 0)
(menu-bar-mode 0)

(provide 'early-init)
