;;; init-org.el -*- lexical-binding: t; -*-

;;; Code:
;;; Deafult

(use-package
  ox-gfm
  :after org)

(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(setq org-log-done nil)
(setq org-startup-truncated nil)

(setq org-directory (expand-file-name "~/.org"))
(setq org-default-notes-file (concat org-directory "/todo.org"))
(setq org-agenda-files (concat org-directory "/agenda.org"))
(setq org-fast-tag-selection-single-key t)
(setq org-use-fast-todo-selection t)

;;; TODO Tags
(setq org-todo-keywords '((sequence "IDEA(i)" "TODO(t)" "STARTED(s)" "NEXT(n)" "WAITING(w)" "|"
				    "DONE(d)")
			  (sequence "|" "CANCELED(c)" "DELEGATED(l)" "SOMEDAY(f)")))

(setq org-todo-keyword-faces '(("IDEA" .
				(:foreground "GoldenRod"
					     :weight bold))
			       ("NEXT" .
				(:foreground "IndianRed1"
					     :weight bold))
			       ("STARTED" .
				(:foreground "OrangeRed"
					     :weight bold))
			       ("WAITING" .
				(:foreground "coral"
					     :weight bold))
			       ("CANCELED" .
				(:foreground "LimeGreen"
					     :weight bold))
			       ("DELEGATED" .
				(:foreground "LimeGreen"
					     :weight bold))
			       ("SOMEDAY" .
				(:foreground "LimeGreen"
					     :weight bold))))

(setq org-tag-persistent-alist
      '((:startgroup .
		     nil)
	("HOME" . ?h)
	("RESEARCH" . ?r)
	("TEACHING" . ?t)
	(:endgroup .
		   nil)
	(:startgroup .
		     nil)
	("OS" . ?o)
	("DEV" . ?d)
	("WWW" . ?w)
	(:endgroup .
		   nil)
	(:startgroup .
		     nil)
	("EASY" . ?e)
	("MEDIUM" . ?m)
	("HARD" . ?a)
	(:endgroup .
		   nil)
	("URGENT" . ?u)
	("KEY" . ?k)
	("BONUS" . ?b)
	("noexport" . ?x)))

(setq org-tag-faces '(("HOME" .
		       (:foreground "GoldenRod"
				    :weight bold))
		      ("RESEARCH" .
		       (:foreground "GoldenRod"
				    :weight bold))
		      ("TEACHING" .
		       (:foreground "GoldenRod"
				    :weight bold))
		      ("OS" .
		       (:foreground "IndianRed1"
				    :weight bold))
		      ("DEV" .
		       (:foreground "IndianRed1"
				    :weight bold))
		      ("WWW" .
		       (:foreground "IndianRed1"
				    :weight bold))
		      ("URGENT" .
		       (:foreground "Red"
				    :weight bold))
		      ("KEY" .
		       (:foreground "Red"
				    :weight bold))
		      ("EASY" .
		       (:foreground "OrangeRed"
				    :weight bold))
		      ("MEDIUM" .
		       (:foreground "OrangeRed"
				    :weight bold))
		      ("HARD" .
		       (:foreground "OrangeRed"
				    :weight bold))
		      ("BONUS" .
		       (:foreground "GoldenRod"
				    :weight bold))
		      ("noexport" .
		       (:foreground "LimeGreen"
				    :weight bold))))


;;; BABEL
(org-babel-do-load-languages 'org-babel-load-languages '((emacs-lisp . t)
							 (org . t)
							 (shell . t)
							 (C . t)
							 (python . t)
							 (clojure . t)
							 (gnuplot . t)
							 (dot . t)
							 (awk . t)))


;;; Export
(setq org-export-with-smart-quotes t)
(setq org-html-coding-system 'utf-8-unix)
(setq org-html-table-default-attributes
      '(:border "0"
		:cellspacing "0"
		:cellpadding "6"
		:rules "none"
		:frame "none"))

;;;;;; Remove all css style in html export
(setq org-html-head-include-default-style nil)
(setq org-html-head-include-scripts nil)

(provide 'init-org)
