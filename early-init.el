;; GC conf
;; (setopt gc-cons-threshold (* (* 1024 1024) 100)) ;; 100MB
;; (setopt read-process-output-max (* 1024 1024))
;; (setopt comp-deferred-compilation t)
;; (defvar comp-deferred-compliation)
;; (add-hook 'after-init-hook #'garbage-collect t)
;; (add-hook 'after-init-hook (lambda () (setopt gc-cons-threshold (* (* 1024 1024) 20)))) ;; back to 20MB

;; please, maximize
(add-to-list 'default-frame-alist '(fullscreen . maximized))
;; quiet startup
;; (setq inhibit-startup-message t)
;; hopefully all themes we install are safe
(setq custom-safe-themes t)
;; load theme
(setq greenized-theme-mixed-fonts nil)
(load-theme 'greenized t)
;; style add padding inside frames (windows)
(add-to-list 'default-frame-alist '(internal-border-width . 2))

(set-face-attribute 'default nil :height 140)

(provide 'early-init)
