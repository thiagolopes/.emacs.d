;;; init-packages.el -*- lexical-binding: t; -*-

;;; Code:

(use-package magit)

(use-package exec-path-from-shell
  :init (setq exec-path-from-shell-arguments nil)
  :config (exec-path-from-shell-initialize))

(use-package undo-tree
  :init (global-undo-tree-mode)
        (defalias 'undo! 'undo-tree-visualize))

(provide 'init-packages)
;;; init-packages ends here
