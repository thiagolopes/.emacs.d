;;; init-base-packages.el -*- lexical-binding: t; -*-

;;; Code:

(use-package magit)

(use-package exec-path-from-shell
  :init (setq exec-path-from-shell-arguments nil)
  :config (exec-path-from-shell-initialize))

(provide 'init-base-packages)
;;; init-base-packages ends here
