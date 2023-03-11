(require 'cl-lib)

(defvar packages-list-p
  '(ace-window
    ag
    browse-kill-ring
    discover-my-major
    diff-hl
    diminish
    easy-kill
    epl
    expand-region
    flycheck
    gist
    git-timemachine
    git-modes
    imenu-anywhere
    magit
    operate-on-number
    smartparens
    smartrep
    super-save
    undo-tree
    which-key
    zop-to-char))

(defun packages-installed-p ()
  (cl-every #'package-installed-p packages-list-p))

(defun require-package (package)
  (unless (memq package packages-list-p)
    (add-to-list 'packages-list-p package))
  (unless (package-installed-p package)
    (package-install package)))

(defun require-packages (packages)
  (mapc #'require-package packages))

(defun install-packages ()
  (unless (packages-installed-p)
    (message "%s" "Emacs is now refreshing its package database...")
    (package-refresh-contents)
    (message "%s" " done.")
    (require-packages packages-list-p)))

;; run package installation
(install-packages)
;; ----------------------------------

(use-package bookmark
  :config
  (setq bookmark-default-file (expand-file-name "bookmarks" savefile-dir)
        bookmark-save-flag 1))

(use-package projectile
  :config
  (setq projectile-cache-file (expand-file-name  "projectile.cache" savefile-dir))
  (projectile-mode t))


