;;; zenburned-theme.el --- A low contrast color theme for Emacs.

;; Copyright (C) 2011-2022 Bozhidar Batsov

;; Original Author: Bozhidar Batsov <bozhidar@batsov.com>
;; Fork Author: Thiago Lopes <thiagolopes@pm.me>
;; Version: 1.0

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; My on fork of zenburn:
;; A port of the popular Vim theme Zenburn for Emacs 24+, built on top
;; of the new built-in theme support in Emacs 24.

;;; Credits:

;; Jani Nurminen created the original theme for vim on which this port
;; is based.

;;; Code:

(deftheme zenburned "The Zenburned color theme.")

(defgroup zenburned-theme nil
  "Zenburned theme."
  :group 'faces
  :prefix "zenburned-"
  :link '(url-link :tag "GitHub" "http://github.com/bbatsov/zenburned-emacs")
  :tag "Zenburned theme")

;;;###autoload
(defcustom zenburned-override-colors-alist '()
  "Place to override default theme colors.

You can override a subset of the theme's default colors by
defining them in this alist."
  :group 'zenburned-theme
  :type '(alist
          :key-type (string :tag "Name")
          :value-type (string :tag " Hex")))

(defvar zenburned-use-variable-pitch nil
  "When non-nil, use variable pitch face for some headings and titles.")

(defvar zenburned-scale-org-headlines nil
  "Whether `org-mode' headlines should be scaled.")

(defvar zenburned-scale-outline-headlines nil
  "Whether `outline-mode' headlines should be scaled.")

(defcustom zenburned-height-minus-1 0.8
  "Font size -1."
  :type 'number
  :group 'zenburned-theme
  :package-version '(zenburned . "2.6"))

(defcustom zenburned-height-plus-1 1.1
  "Font size +1."
  :type 'number
  :group 'zenburned-theme
  :package-version '(zenburned . "2.6"))

(defcustom zenburned-height-plus-2 1.15
  "Font size +2."
  :type 'number
  :group 'zenburned-theme
  :package-version '(zenburned . "2.6"))

(defcustom zenburned-height-plus-3 1.2
  "Font size +3."
  :type 'number
  :group 'zenburned-theme
  :package-version '(zenburned . "2.6"))

(defcustom zenburned-height-plus-4 1.3
  "Font size +4."
  :type 'number
  :group 'zenburned-theme
  :package-version '(zenburned . "2.6"))

;;; Color Palette

(defvar zenburned-default-colors-alist
  '(("zenburned-fg-1"     . "#656555")
    ("zenburned-fg-05"    . "#989890")
    ("zenburned-fg"       . "#DCDCCC")
    ("zenburned-fg+1"     . "#FFFFEF")
    ("zenburned-fg+2"     . "#FFFFFD")
    ("zenburned-bg-2"     . "#000000")
    ("zenburned-bg-1"     . "#2B2B2B")
    ("zenburned-bg-08"    . "#303030")
    ("zenburned-bg-05"    . "#383838")
    ("zenburned-bg"       . "#3F3F3F")
    ("zenburned-bg+05"    . "#494949")
    ("zenburned-bg+1"     . "#4F4F4F")
    ("zenburned-bg+2"     . "#5F5F5F")
    ("zenburned-bg+3"     . "#6F6F6F")
    ("zenburned-red-6"    . "#6C3333")
    ("zenburned-red-5"    . "#7C4343")
    ("zenburned-red-4"    . "#8C5353")
    ("zenburned-red-3"    . "#9C6363")
    ("zenburned-red-2"    . "#AC7373")
    ("zenburned-red-1"    . "#BC8383")
    ("zenburned-red"      . "#CC9393")
    ("zenburned-red+1"    . "#DCA3A3")
    ("zenburned-red+2"    . "#ECB3B3")
    ("zenburned-orange"   . "#DFAF8F")
    ("zenburned-yellow-2" . "#D0BF8F")
    ("zenburned-yellow-1" . "#E0CF9F")
    ("zenburned-yellow"   . "#F0DFAF")
    ("zenburned-green-5"  . "#2F4F2F")
    ("zenburned-green-4"  . "#3F5F3F")
    ("zenburned-green-3"  . "#4F6F4F")
    ("zenburned-green-2"  . "#5F7F5F")
    ("zenburned-green-1"  . "#6F8F6F")
    ("zenburned-green"    . "#7F9F7F")
    ("zenburned-green+1"  . "#8FB28F")
    ("zenburned-green+2"  . "#9FC59F")
    ("zenburned-green+3"  . "#AFD8AF")
    ("zenburned-green+4"  . "#BFEBBF")
    ("zenburned-cyan"     . "#93E0E3")
    ("zenburned-blue+3"   . "#BDE0F3")
    ("zenburned-blue+2"   . "#ACE0E3")
    ("zenburned-blue+1"   . "#94BFF3")
    ("zenburned-blue"     . "#8CD0D3")
    ("zenburned-blue-1"   . "#7CB8BB")
    ("zenburned-blue-2"   . "#6CA0A3")
    ("zenburned-blue-3"   . "#5C888B")
    ("zenburned-blue-4"   . "#4C7073")
    ("zenburned-blue-5"   . "#366060")
    ("zenburned-magenta"  . "#DC8CC3"))
  "List of Zenburned colors.
Each element has the form (NAME . HEX).

`+N' suffixes indicate a color is lighter.
`-N' suffixes indicate a color is darker.")

(defmacro zenburned-with-color-variables (&rest body)
  "`let' bind all colors defined in `zenburned-colors-alist' around BODY.
Also bind `class' to ((class color) (min-colors 89))."
  (declare (indent 0))
  `(let ((class '((class color) (min-colors 89)))
         ,@(mapcar (lambda (cons)
                     (list (intern (car cons)) (cdr cons)))
                   (append zenburned-default-colors-alist
                           zenburned-override-colors-alist))
         (z-variable-pitch (if zenburned-use-variable-pitch
                               'variable-pitch 'default)))
     ,@body))

;;; Theme Faces
(zenburned-with-color-variables
  (custom-theme-set-faces
   'zenburned

;;;; Built-in packages

;;;;; basic coloring
   '(button ((t (:underline t))))
   `(link ((t (:foreground ,zenburned-yellow :underline t :weight bold))))
   `(link-visited ((t (:foreground ,zenburned-yellow-2 :underline t :weight normal))))
   `(default ((t (:foreground ,zenburned-fg :background ,zenburned-bg))))
   `(cursor ((t (:foreground ,zenburned-fg :background ,zenburned-fg+1))))
   `(widget-field ((t (:foreground ,zenburned-fg :background ,zenburned-bg+3))))
   `(escape-glyph ((t (:foreground ,zenburned-yellow :weight bold))))
   `(fringe ((t (:foreground ,zenburned-fg :background ,zenburned-bg))))
   `(header-line ((t (:foreground ,zenburned-yellow
                                  :background ,zenburned-bg-1
                                  :box (:line-width -1 :style released-button)
                                  :extend t))))
   `(highlight ((t (:background ,zenburned-bg-05))))
   `(success ((t (:foreground ,zenburned-green :weight bold))))
   `(warning ((t (:foreground ,zenburned-orange :weight bold))))
   `(tooltip ((t (:foreground ,zenburned-fg :background ,zenburned-bg+1))))
;;;;; ansi-colors
   `(ansi-color-black ((t (:foreground ,zenburned-bg
                                       :background ,zenburned-bg-1))))
   `(ansi-color-red ((t (:foreground ,zenburned-red-2
                                     :background ,zenburned-red-4))))
   `(ansi-color-green ((t (:foreground ,zenburned-green
                                       :background ,zenburned-green+2))))
   `(ansi-color-yellow ((t (:foreground ,zenburned-orange
                                        :background ,zenburned-yellow))))
   `(ansi-color-blue ((t (:foreground ,zenburned-blue-1
                                      :background ,zenburned-blue-4))))
   `(ansi-color-magenta ((t (:foreground ,zenburned-magenta
                                         :background ,zenburned-red))))
   `(ansi-color-cyan ((t (:foreground ,zenburned-cyan
                                      :background ,zenburned-blue))))
   `(ansi-color-white ((t (:foreground ,zenburned-fg
                                       :background ,zenburned-fg-1))))
;;;;; compilation
   `(compilation-column-face ((t (:foreground ,zenburned-yellow))))
   `(compilation-enter-directory-face ((t (:foreground ,zenburned-green))))
   `(compilation-error-face ((t (:foreground ,zenburned-red-1 :weight bold :underline t))))
   `(compilation-face ((t (:foreground ,zenburned-fg))))
   `(compilation-info-face ((t (:foreground ,zenburned-blue))))
   `(compilation-info ((t (:foreground ,zenburned-green+4 :underline t))))
   `(compilation-leave-directory-face ((t (:foreground ,zenburned-green))))
   `(compilation-line-face ((t (:foreground ,zenburned-yellow))))
   `(compilation-line-number ((t (:foreground ,zenburned-yellow))))
   `(compilation-message-face ((t (:foreground ,zenburned-blue))))
   `(compilation-warning-face ((t (:foreground ,zenburned-orange :weight bold :underline t))))
   `(compilation-mode-line-exit ((t (:foreground ,zenburned-green+2 :weight bold))))
   `(compilation-mode-line-fail ((t (:foreground ,zenburned-red :weight bold))))
   `(compilation-mode-line-run ((t (:foreground ,zenburned-yellow :weight bold))))
;;;;; completions
   `(completions-annotations ((t (:foreground ,zenburned-fg-1))))
   `(completions-common-part ((t (:foreground ,zenburned-blue))))
   `(completions-first-difference ((t (:foreground ,zenburned-fg+1))))
;;;;; customize
   `(custom-variable-tag ((t (:foreground ,zenburned-blue :weight bold))))
   `(custom-group-tag ((t (:foreground ,zenburned-blue :weight bold :height 1.2))))
   `(custom-state ((t (:foreground ,zenburned-green+4))))
;;;;; display-fill-column-indicator
   `(fill-column-indicator ((,class :foreground ,zenburned-bg-05 :weight semilight)))
;;;;; eww
   '(eww-invalid-certificate ((t (:inherit error))))
   '(eww-valid-certificate   ((t (:inherit success))))
;;;;; grep
   `(grep-context-face ((t (:foreground ,zenburned-fg))))
   `(grep-error-face ((t (:foreground ,zenburned-red-1 :weight bold :underline t))))
   `(grep-hit-face ((t (:foreground ,zenburned-blue))))
   `(grep-match-face ((t (:foreground ,zenburned-orange :weight bold))))
   `(match ((t (:background ,zenburned-bg-1 :foreground ,zenburned-orange :weight bold))))
;;;;; hi-lock
   `(hi-blue    ((t (:background ,zenburned-cyan    :foreground ,zenburned-bg-1))))
   `(hi-green   ((t (:background ,zenburned-green+4 :foreground ,zenburned-bg-1))))
   `(hi-pink    ((t (:background ,zenburned-magenta :foreground ,zenburned-bg-1))))
   `(hi-yellow  ((t (:background ,zenburned-yellow  :foreground ,zenburned-bg-1))))
   `(hi-blue-b  ((t (:foreground ,zenburned-blue    :weight     bold))))
   `(hi-green-b ((t (:foreground ,zenburned-green+2 :weight     bold))))
   `(hi-red-b   ((t (:foreground ,zenburned-red     :weight     bold))))
;;;;; info
   `(Info-quoted ((t (:inherit font-lock-constant-face))))
;;;;; isearch
   `(isearch ((t (:foreground ,zenburned-yellow-2 :weight bold :background ,zenburned-bg+2))))
   `(isearch-fail ((t (:foreground ,zenburned-fg :background ,zenburned-red-4))))
   `(lazy-highlight ((t (:foreground ,zenburned-yellow-2 :weight bold :background ,zenburned-bg-05))))

   `(menu ((t (:foreground ,zenburned-fg :background ,zenburned-bg))))
   `(minibuffer-prompt ((t (:foreground ,zenburned-yellow))))
   `(mode-line
     ((,class (:foreground ,zenburned-green+1
                           :background ,zenburned-bg-1
                           :box (:line-width -1 :style released-button)))
      (t :inverse-video t)))
   `(mode-line-buffer-id ((t (:foreground ,zenburned-yellow :weight bold))))
   `(mode-line-inactive
     ((t (:foreground ,zenburned-green-2
                      :background ,zenburned-bg-05
                      :box (:line-width -1 :style released-button)))))
   `(region ((,class (:background ,zenburned-bg-1 :extend t))
             (t :inverse-video t)))
   `(secondary-selection ((t (:background ,zenburned-bg+2))))
   `(trailing-whitespace ((t (:background ,zenburned-red))))
   `(vertical-border ((t (:foreground ,zenburned-fg))))
;;;;; font lock
   `(font-lock-builtin-face ((t (:foreground ,zenburned-yellow :slant italic))))
   `(font-lock-comment-face ((t (:foreground ,zenburned-green-1 :background ,zenburned-bg-1))))
   `(font-lock-comment-delimiter-face ((t (:foreground ,zenburned-green-1 :background ,zenburned-bg-1))))
   `(font-lock-constant-face ((t (:foreground ,zenburned-fg))))
   `(font-lock-doc-face ((t (:foreground ,zenburned-green+2))))
   `(font-lock-function-name-face ((t (:foreground ,zenburned-cyan))))
   `(font-lock-keyword-face ((t (:foreground ,zenburned-yellow :slant italic))))
   `(font-lock-negation-char-face ((t (:foreground ,zenburned-yellow))))
   `(font-lock-preprocessor-face ((t (:foreground ,zenburned-blue+1))))
   `(font-lock-regexp-grouping-construct ((t (:foreground ,zenburned-yellow))))
   `(font-lock-regexp-grouping-backslash ((t (:foreground ,zenburned-green))))
   `(font-lock-string-face ((t (:foreground ,zenburned-green))))
   `(font-lock-type-face ((t (:foreground ,zenburned-blue-1))))
   `(font-lock-variable-name-face ((t (:foreground ,zenburned-fg))))
   `(font-lock-warning-face ((t (:foreground ,zenburned-yellow-2))))

   `(c-annotation-face ((t (:inherit font-lock-constant-face))))
;;;;; line numbers (Emacs 26.1 and above)
   `(line-number ((t (:inherit default :foreground ,zenburned-bg+3 :background ,zenburned-bg))))
   `(line-number-current-line ((t (:inherit line-number :foreground ,zenburned-yellow-2))))
;;;;; man
   '(Man-overstrike ((t (:inherit font-lock-keyword-face))))
   '(Man-underline  ((t (:inherit (font-lock-string-face underline)))))
;;;;; newsticker
   `(newsticker-date-face ((t (:foreground ,zenburned-fg))))
   `(newsticker-default-face ((t (:foreground ,zenburned-fg))))
   `(newsticker-enclosure-face ((t (:foreground ,zenburned-green+3))))
   `(newsticker-extra-face ((t (:foreground ,zenburned-bg+2 :height 0.8))))
   `(newsticker-feed-face ((t (:foreground ,zenburned-fg))))
   `(newsticker-immortal-item-face ((t (:foreground ,zenburned-green))))
   `(newsticker-new-item-face ((t (:foreground ,zenburned-blue))))
   `(newsticker-obsolete-item-face ((t (:foreground ,zenburned-red))))
   `(newsticker-old-item-face ((t (:foreground ,zenburned-bg+3))))
   `(newsticker-statistics-face ((t (:foreground ,zenburned-fg))))
   `(newsticker-treeview-face ((t (:foreground ,zenburned-fg))))
   `(newsticker-treeview-immortal-face ((t (:foreground ,zenburned-green))))
   `(newsticker-treeview-listwindow-face ((t (:foreground ,zenburned-fg))))
   `(newsticker-treeview-new-face ((t (:foreground ,zenburned-blue :weight bold))))
   `(newsticker-treeview-obsolete-face ((t (:foreground ,zenburned-red))))
   `(newsticker-treeview-old-face ((t (:foreground ,zenburned-bg+3))))
   `(newsticker-treeview-selection-face ((t (:background ,zenburned-bg-1 :foreground ,zenburned-yellow))))
;;;;; woman
   '(woman-bold   ((t (:inherit font-lock-keyword-face))))
   '(woman-italic ((t (:inherit (font-lock-string-face italic)))))

;;;; Third-party packages

;;;;; ace-jump
   `(ace-jump-face-background
     ((t (:foreground ,zenburned-fg-1 :background ,zenburned-bg :inverse-video nil))))
   `(ace-jump-face-foreground
     ((t (:foreground ,zenburned-green+2 :background ,zenburned-bg :inverse-video nil))))
;;;;; ace-window
   `(aw-background-face
     ((t (:foreground ,zenburned-fg-1 :background ,zenburned-bg :inverse-video nil))))
   `(aw-leading-char-face ((t (:inherit aw-mode-line-face))))
;;;;; adoc-mode
   `(adoc-anchor-face ((t (:foreground ,zenburned-blue+1))))
   `(adoc-code-face ((t (:inherit font-lock-constant-face))))
   `(adoc-command-face ((t (:foreground ,zenburned-yellow))))
   `(adoc-emphasis-face ((t (:inherit bold))))
   `(adoc-internal-reference-face ((t (:foreground ,zenburned-yellow-2 :underline t))))
   `(adoc-list-face ((t (:foreground ,zenburned-fg+1))))
   `(adoc-meta-face ((t (:foreground ,zenburned-yellow))))
   `(adoc-meta-hide-face ((t (:foreground ,zenburned-yellow))))
   `(adoc-secondary-text-face ((t (:foreground ,zenburned-yellow-1))))
   `(adoc-title-0-face ((t (:inherit font-lock-function-name-face :weight bold))))
   `(adoc-title-1-face ((t (:inherit font-lock-function-name-face :weight bold))))
   `(adoc-title-2-face ((t (:inherit font-lock-function-name-face :weight bold))))
   `(adoc-title-3-face ((t (:inherit font-lock-function-name-face :weight bold))))
   `(adoc-title-4-face ((t (:inherit font-lock-function-name-face :weight bold))))
   `(adoc-typewriter-face ((t (:inherit font-lock-constant-face))))
   `(adoc-verbatim-face ((t (:inherit font-lock-constant-face))))
   `(adoc-value-face ((t (:foreground ,zenburned-yellow))))
;;;;; android mode
   `(android-mode-debug-face ((t (:foreground ,zenburned-green+1))))
   `(android-mode-error-face ((t (:foreground ,zenburned-orange :weight bold))))
   `(android-mode-info-face ((t (:foreground ,zenburned-fg))))
   `(android-mode-verbose-face ((t (:foreground ,zenburned-green))))
   `(android-mode-warning-face ((t (:foreground ,zenburned-yellow))))
;;;;; anzu
   `(anzu-mode-line ((t (:foreground ,zenburned-cyan :weight bold))))
   `(anzu-mode-line-no-match ((t (:foreground ,zenburned-red :weight bold))))
   `(anzu-match-1 ((t (:foreground ,zenburned-bg :background ,zenburned-green))))
   `(anzu-match-2 ((t (:foreground ,zenburned-bg :background ,zenburned-orange))))
   `(anzu-match-3 ((t (:foreground ,zenburned-bg :background ,zenburned-blue))))
   `(anzu-replace-to ((t (:inherit anzu-replace-highlight :foreground ,zenburned-yellow))))
;;;;; auctex
   `(font-latex-bold-face ((t (:inherit bold))))
   `(font-latex-warning-face ((t (:foreground nil :inherit font-lock-warning-face))))
   `(font-latex-sectioning-5-face ((t (:foreground ,zenburned-red :weight bold ))))
   `(font-latex-sedate-face ((t (:foreground ,zenburned-yellow))))
   `(font-latex-italic-face ((t (:foreground ,zenburned-cyan :slant italic))))
   `(font-latex-string-face ((t (:inherit ,font-lock-string-face))))
   `(font-latex-math-face ((t (:foreground ,zenburned-orange))))
   `(font-latex-script-char-face ((t (:foreground ,zenburned-orange))))
;;;;; agda-mode
   `(agda2-highlight-keyword-face ((t (:foreground ,zenburned-yellow :weight bold))))
   `(agda2-highlight-string-face ((t (:foreground ,zenburned-red))))
   `(agda2-highlight-symbol-face ((t (:foreground ,zenburned-orange))))
   `(agda2-highlight-primitive-type-face ((t (:foreground ,zenburned-blue-1))))
   `(agda2-highlight-inductive-constructor-face ((t (:foreground ,zenburned-fg))))
   `(agda2-highlight-coinductive-constructor-face ((t (:foreground ,zenburned-fg))))
   `(agda2-highlight-datatype-face ((t (:foreground ,zenburned-blue))))
   `(agda2-highlight-function-face ((t (:foreground ,zenburned-blue))))
   `(agda2-highlight-module-face ((t (:foreground ,zenburned-blue-1))))
   `(agda2-highlight-error-face ((t (:foreground ,zenburned-bg :background ,zenburned-magenta))))
   `(agda2-highlight-unsolved-meta-face ((t (:foreground ,zenburned-bg :background ,zenburned-magenta))))
   `(agda2-highlight-unsolved-constraint-face ((t (:foreground ,zenburned-bg :background ,zenburned-magenta))))
   `(agda2-highlight-termination-problem-face ((t (:foreground ,zenburned-bg :background ,zenburned-magenta))))
   `(agda2-highlight-incomplete-pattern-face ((t (:foreground ,zenburned-bg :background ,zenburned-magenta))))
   `(agda2-highlight-typechecks-face ((t (:background ,zenburned-red-4))))
;;;;; auto-complete
   `(ac-candidate-face ((t (:background ,zenburned-bg+3 :foreground ,zenburned-bg-2))))
   `(ac-selection-face ((t (:background ,zenburned-blue-4 :foreground ,zenburned-fg))))
   `(popup-tip-face ((t (:background ,zenburned-yellow-2 :foreground ,zenburned-bg-2))))
   `(popup-menu-mouse-face ((t (:background ,zenburned-yellow-2 :foreground ,zenburned-bg-2))))
   `(popup-summary-face ((t (:background ,zenburned-bg+3 :foreground ,zenburned-bg-2))))
   `(popup-scroll-bar-foreground-face ((t (:background ,zenburned-blue-5))))
   `(popup-scroll-bar-background-face ((t (:background ,zenburned-bg-1))))
   `(popup-isearch-match ((t (:background ,zenburned-bg :foreground ,zenburned-fg))))
;;;;; avy
   `(avy-background-face
     ((t (:foreground ,zenburned-fg-1 :background ,zenburned-bg :inverse-video nil))))
   `(avy-lead-face-0
     ((t (:foreground ,zenburned-green+3 :background ,zenburned-bg :inverse-video nil :weight bold))))
   `(avy-lead-face-1
     ((t (:foreground ,zenburned-yellow :background ,zenburned-bg :inverse-video nil :weight bold))))
   `(avy-lead-face-2
     ((t (:foreground ,zenburned-red+1 :background ,zenburned-bg :inverse-video nil :weight bold))))
   `(avy-lead-face
     ((t (:foreground ,zenburned-cyan :background ,zenburned-bg :inverse-video nil :weight bold))))
;;;;; company-mode
   `(company-tooltip ((t (:foreground ,zenburned-fg :background ,zenburned-bg+1))))
   `(company-tooltip-annotation ((t (:foreground ,zenburned-orange :background ,zenburned-bg+1))))
   `(company-tooltip-annotation-selection ((t (:foreground ,zenburned-orange :background ,zenburned-bg-1))))
   `(company-tooltip-selection ((t (:foreground ,zenburned-fg :background ,zenburned-bg-1))))
   `(company-tooltip-mouse ((t (:background ,zenburned-bg-1))))
   `(company-tooltip-common ((t (:foreground ,zenburned-green+2))))
   `(company-tooltip-common-selection ((t (:foreground ,zenburned-green+2))))
   `(company-scrollbar-fg ((t (:background ,zenburned-bg-1))))
   `(company-scrollbar-bg ((t (:background ,zenburned-bg+2))))
   `(company-preview ((t (:background ,zenburned-green+2))))
   `(company-preview-common ((t (:foreground ,zenburned-green+2 :background ,zenburned-bg-1))))
;;;;; corfu
   `(corfu-default ((t (:foreground ,zenburned-fg :background ,zenburned-bg+1))))
   `(corfu-current ((t (:foreground ,zenburned-fg :background ,zenburned-bg-1))))
   `(corfu-bar ((t (:background ,zenburned-bg-1))))
   `(corfu-bar ((t (:background ,zenburned-bg-2))))
;;;;; bm
   `(bm-face ((t (:background ,zenburned-yellow-1 :foreground ,zenburned-bg))))
   `(bm-fringe-face ((t (:background ,zenburned-yellow-1 :foreground ,zenburned-bg))))
   `(bm-fringe-persistent-face ((t (:background ,zenburned-green-2 :foreground ,zenburned-bg))))
   `(bm-persistent-face ((t (:background ,zenburned-green-2 :foreground ,zenburned-bg))))
;;;;; calfw
   `(cfw:face-annotation ((t (:foreground ,zenburned-red :inherit cfw:face-day-title))))
   `(cfw:face-day-title ((t nil)))
   `(cfw:face-default-content ((t (:foreground ,zenburned-green))))
   `(cfw:face-default-day ((t (:weight bold))))
   `(cfw:face-disable ((t (:foreground ,zenburned-fg-1))))
   `(cfw:face-grid ((t (:inherit shadow))))
   `(cfw:face-header ((t (:inherit font-lock-keyword-face))))
   `(cfw:face-holiday ((t (:inherit cfw:face-sunday))))
   `(cfw:face-periods ((t (:foreground ,zenburned-cyan))))
   `(cfw:face-saturday ((t (:foreground ,zenburned-blue :weight bold))))
   `(cfw:face-select ((t (:background ,zenburned-blue-5))))
   `(cfw:face-sunday ((t (:foreground ,zenburned-red :weight bold))))
   `(cfw:face-title ((t (:height 2.0 :inherit (variable-pitch font-lock-keyword-face)))))
   `(cfw:face-today ((t (:foreground ,zenburned-cyan :weight bold))))
   `(cfw:face-today-title ((t (:inherit highlight bold))))
   `(cfw:face-toolbar ((t (:background ,zenburned-blue-5))))
   `(cfw:face-toolbar-button-off ((t (:underline nil :inherit link))))
   `(cfw:face-toolbar-button-on ((t (:underline nil :inherit link-visited))))
;;;;; centaur-tabs
   `(centaur-tabs-default ((t (:background ,zenburned-bg :foreground ,zenburned-fg :box nil))))
   `(centaur-tabs-selected ((t (:background ,zenburned-bg :foreground ,zenburned-fg+2 :box nil))))
   `(centaur-tabs-unselected ((t (:background ,zenburned-bg-1 :foreground ,zenburned-fg-05 :box nil))))
   `(centaur-tabs-selected-modified ((t (:background ,zenburned-bg :foreground ,zenburned-orange :box nil))))
   `(centaur-tabs-unselected-modified ((t (:background ,zenburned-bg-1 :foreground ,zenburned-orange :box nil))))
   `(centaur-tabs-active-bar-face ((t (:background ,zenburned-yellow :box nil))))
   `(centaur-tabs-modified-marker-selected ((t (:inherit 'centaur-tabs-selected-modified :foreground ,zenburned-yellow :box nil))))
   `(centaur-tabs-modified-marker-unselected ((t (:inherit 'centaur-tabs-unselected-modified :foreground ,zenburned-yellow :box nil))))
;;;;; cider
   `(cider-result-overlay-face ((t (:background unspecified))))
   `(cider-enlightened-face ((t (:box (:color ,zenburned-orange :line-width -1)))))
   `(cider-enlightened-local-face ((t (:weight bold :foreground ,zenburned-green+1))))
   `(cider-deprecated-face ((t (:background ,zenburned-yellow-2))))
   `(cider-instrumented-face ((t (:box (:color ,zenburned-red :line-width -1)))))
   `(cider-traced-face ((t (:box (:color ,zenburned-cyan :line-width -1)))))
   `(cider-test-failure-face ((t (:background ,zenburned-red-4))))
   `(cider-test-error-face ((t (:background ,zenburned-magenta))))
   `(cider-test-success-face ((t (:background ,zenburned-green-2))))
   `(cider-fringe-good-face ((t (:foreground ,zenburned-green+4))))
;;;;; circe
   `(circe-highlight-nick-face ((t (:foreground ,zenburned-cyan))))
   `(circe-my-message-face ((t (:foreground ,zenburned-fg))))
   `(circe-fool-face ((t (:foreground ,zenburned-red+1))))
   `(circe-topic-diff-removed-face ((t (:foreground ,zenburned-red :weight bold))))
   `(circe-originator-face ((t (:foreground ,zenburned-fg))))
   `(circe-server-face ((t (:foreground ,zenburned-green))))
   `(circe-topic-diff-new-face ((t (:foreground ,zenburned-orange :weight bold))))
   `(circe-prompt-face ((t (:foreground ,zenburned-orange :background ,zenburned-bg :weight bold))))
;;;;; context-coloring
   `(context-coloring-level-0-face ((t :foreground ,zenburned-fg)))
   `(context-coloring-level-1-face ((t :foreground ,zenburned-cyan)))
   `(context-coloring-level-2-face ((t :foreground ,zenburned-green+4)))
   `(context-coloring-level-3-face ((t :foreground ,zenburned-yellow)))
   `(context-coloring-level-4-face ((t :foreground ,zenburned-orange)))
   `(context-coloring-level-5-face ((t :foreground ,zenburned-magenta)))
   `(context-coloring-level-6-face ((t :foreground ,zenburned-blue+1)))
   `(context-coloring-level-7-face ((t :foreground ,zenburned-green+2)))
   `(context-coloring-level-8-face ((t :foreground ,zenburned-yellow-2)))
   `(context-coloring-level-9-face ((t :foreground ,zenburned-red+1)))
;;;;; coq
   `(coq-solve-tactics-face ((t (:foreground nil :inherit font-lock-constant-face))))
;;;;; ctable
   `(ctbl:face-cell-select ((t (:background ,zenburned-blue :foreground ,zenburned-bg))))
   `(ctbl:face-continue-bar ((t (:background ,zenburned-bg-05 :foreground ,zenburned-bg))))
   `(ctbl:face-row-select ((t (:background ,zenburned-cyan :foreground ,zenburned-bg))))
;;;;; debbugs
   `(debbugs-gnu-done ((t (:foreground ,zenburned-fg-1))))
   `(debbugs-gnu-handled ((t (:foreground ,zenburned-green))))
   `(debbugs-gnu-new ((t (:foreground ,zenburned-red))))
   `(debbugs-gnu-pending ((t (:foreground ,zenburned-blue))))
   `(debbugs-gnu-stale ((t (:foreground ,zenburned-orange))))
   `(debbugs-gnu-tagged ((t (:foreground ,zenburned-red))))
;;;;; diff
   ;; Please read (info "(magit)Theming Faces") before changing this.
   `(diff-added          ((t (:background "#335533" :foreground ,zenburned-green))))
   `(diff-changed        ((t (:background "#555511" :foreground ,zenburned-yellow-1))))
   `(diff-removed        ((t (:background "#553333" :foreground ,zenburned-red-2))))
   `(diff-refine-added   ((t (:background "#338833" :foreground ,zenburned-green+4))))
   `(diff-refine-changed ((t (:background "#888811" :foreground ,zenburned-yellow))))
   `(diff-refine-removed ((t (:background "#883333" :foreground ,zenburned-red))))
   `(diff-header ((,class (:background ,zenburned-bg+2))
                  (t (:background ,zenburned-fg :foreground ,zenburned-bg))))
   `(diff-file-header
     ((,class (:background ,zenburned-bg+2 :foreground ,zenburned-fg :weight bold))
      (t (:background ,zenburned-fg :foreground ,zenburned-bg :weight bold))))
;;;;; diff-hl
   `(diff-hl-change ((,class (:foreground ,zenburned-blue :background ,zenburned-blue-2))))
   `(diff-hl-delete ((,class (:foreground ,zenburned-red+1 :background ,zenburned-red-1))))
   `(diff-hl-insert ((,class (:foreground ,zenburned-green+1 :background ,zenburned-green-2))))
;;;;; dim-autoload
   `(dim-autoload-cookie-line ((t :foreground ,zenburned-bg+1)))
;;;;; dired+
   `(diredp-display-msg ((t (:foreground ,zenburned-blue))))
   `(diredp-compressed-file-suffix ((t (:foreground ,zenburned-orange))))
   `(diredp-date-time ((t (:foreground ,zenburned-magenta))))
   `(diredp-deletion ((t (:foreground ,zenburned-yellow))))
   `(diredp-deletion-file-name ((t (:foreground ,zenburned-red))))
   `(diredp-dir-heading ((t (:foreground ,zenburned-blue :background ,zenburned-bg-1))))
   `(diredp-dir-priv ((t (:foreground ,zenburned-cyan))))
   `(diredp-exec-priv ((t (:foreground ,zenburned-red))))
   `(diredp-executable-tag ((t (:foreground ,zenburned-green+1))))
   `(diredp-file-name ((t (:foreground ,zenburned-blue))))
   `(diredp-file-suffix ((t (:foreground ,zenburned-green))))
   `(diredp-flag-mark ((t (:foreground ,zenburned-yellow))))
   `(diredp-flag-mark-line ((t (:foreground ,zenburned-orange))))
   `(diredp-ignored-file-name ((t (:foreground ,zenburned-red))))
   `(diredp-link-priv ((t (:foreground ,zenburned-yellow))))
   `(diredp-mode-line-flagged ((t (:foreground ,zenburned-yellow))))
   `(diredp-mode-line-marked ((t (:foreground ,zenburned-orange))))
   `(diredp-no-priv ((t (:foreground ,zenburned-fg))))
   `(diredp-number ((t (:foreground ,zenburned-green+1))))
   `(diredp-other-priv ((t (:foreground ,zenburned-yellow-1))))
   `(diredp-rare-priv ((t (:foreground ,zenburned-red-1))))
   `(diredp-read-priv ((t (:foreground ,zenburned-green-2))))
   `(diredp-symlink ((t (:foreground ,zenburned-yellow))))
   `(diredp-write-priv ((t (:foreground ,zenburned-magenta))))
;;;;; dired-async
   `(dired-async-failures ((t (:foreground ,zenburned-red :weight bold))))
   `(dired-async-message ((t (:foreground ,zenburned-yellow :weight bold))))
   `(dired-async-mode-message ((t (:foreground ,zenburned-yellow))))
;;;;; diredfl
   `(diredfl-compressed-file-suffix ((t (:foreground ,zenburned-orange))))
   `(diredfl-date-time ((t (:foreground ,zenburned-magenta))))
   `(diredfl-deletion ((t (:foreground ,zenburned-yellow))))
   `(diredfl-deletion-file-name ((t (:foreground ,zenburned-red))))
   `(diredfl-dir-heading ((t (:foreground ,zenburned-blue :background ,zenburned-bg-1))))
   `(diredfl-dir-priv ((t (:foreground ,zenburned-cyan))))
   `(diredfl-exec-priv ((t (:foreground ,zenburned-red))))
   `(diredfl-executable-tag ((t (:foreground ,zenburned-green+1))))
   `(diredfl-file-name ((t (:foreground ,zenburned-blue))))
   `(diredfl-file-suffix ((t (:foreground ,zenburned-green))))
   `(diredfl-flag-mark ((t (:foreground ,zenburned-yellow))))
   `(diredfl-flag-mark-line ((t (:foreground ,zenburned-orange))))
   `(diredfl-ignored-file-name ((t (:foreground ,zenburned-red))))
   `(diredfl-link-priv ((t (:foreground ,zenburned-yellow))))
   `(diredfl-no-priv ((t (:foreground ,zenburned-fg))))
   `(diredfl-number ((t (:foreground ,zenburned-green+1))))
   `(diredfl-other-priv ((t (:foreground ,zenburned-yellow-1))))
   `(diredfl-rare-priv ((t (:foreground ,zenburned-red-1))))
   `(diredfl-read-priv ((t (:foreground ,zenburned-green-1))))
   `(diredfl-symlink ((t (:foreground ,zenburned-yellow))))
   `(diredfl-write-priv ((t (:foreground ,zenburned-magenta))))
;;;;; doom-modeline
   `(doom-modeline-bar  ((t (:background ,zenburned-yellow))))
   `(doom-modeline-inactive-bar  ((t (:background nil))))
;;;;; ediff
   `(ediff-current-diff-A ((t (:foreground ,zenburned-fg :background ,zenburned-red-4))))
   `(ediff-current-diff-Ancestor ((t (:foreground ,zenburned-fg :background ,zenburned-red-4))))
   `(ediff-current-diff-B ((t (:foreground ,zenburned-fg :background ,zenburned-green-2))))
   `(ediff-current-diff-C ((t (:foreground ,zenburned-fg :background ,zenburned-blue-5))))
   `(ediff-even-diff-A ((t (:background ,zenburned-bg+1))))
   `(ediff-even-diff-Ancestor ((t (:background ,zenburned-bg+1))))
   `(ediff-even-diff-B ((t (:background ,zenburned-bg+1))))
   `(ediff-even-diff-C ((t (:background ,zenburned-bg+1))))
   `(ediff-fine-diff-A ((t (:foreground ,zenburned-fg :background ,zenburned-red-2 :weight bold))))
   `(ediff-fine-diff-Ancestor ((t (:foreground ,zenburned-fg :background ,zenburned-red-2 weight bold))))
   `(ediff-fine-diff-B ((t (:foreground ,zenburned-fg :background ,zenburned-green :weight bold))))
   `(ediff-fine-diff-C ((t (:foreground ,zenburned-fg :background ,zenburned-blue-3 :weight bold ))))
   `(ediff-odd-diff-A ((t (:background ,zenburned-bg+2))))
   `(ediff-odd-diff-Ancestor ((t (:background ,zenburned-bg+2))))
   `(ediff-odd-diff-B ((t (:background ,zenburned-bg+2))))
   `(ediff-odd-diff-C ((t (:background ,zenburned-bg+2))))
;;;;; egg
   `(egg-text-base ((t (:foreground ,zenburned-fg))))
   `(egg-help-header-1 ((t (:foreground ,zenburned-yellow))))
   `(egg-help-header-2 ((t (:foreground ,zenburned-green+3))))
   `(egg-branch ((t (:foreground ,zenburned-yellow))))
   `(egg-branch-mono ((t (:foreground ,zenburned-yellow))))
   `(egg-term ((t (:foreground ,zenburned-yellow))))
   `(egg-diff-add ((t (:foreground ,zenburned-green+4))))
   `(egg-diff-del ((t (:foreground ,zenburned-red+1))))
   `(egg-diff-file-header ((t (:foreground ,zenburned-yellow-2))))
   `(egg-section-title ((t (:foreground ,zenburned-yellow))))
   `(egg-stash-mono ((t (:foreground ,zenburned-green+4))))
;;;;; elfeed
   `(elfeed-log-error-level-face ((t (:foreground ,zenburned-red))))
   `(elfeed-log-info-level-face ((t (:foreground ,zenburned-blue))))
   `(elfeed-log-warn-level-face ((t (:foreground ,zenburned-yellow))))
   `(elfeed-search-date-face ((t (:foreground ,zenburned-yellow-1 :underline t
                                              :weight bold))))
   `(elfeed-search-tag-face ((t (:foreground ,zenburned-green))))
   `(elfeed-search-feed-face ((t (:foreground ,zenburned-cyan))))
   `(elfeed-search-title-face ((t (:foreground ,zenburned-fg-05))))
   `(elfeed-search-unread-title-face ((t (:foreground ,zenburned-fg :weight bold))))
;;;;; emacs-w3m
   `(w3m-anchor ((t (:foreground ,zenburned-yellow :underline t
                                 :weight bold))))
   `(w3m-arrived-anchor ((t (:foreground ,zenburned-yellow-2
                                         :underline t :weight normal))))
   `(w3m-form ((t (:foreground ,zenburned-red-1 :underline t))))
   `(w3m-header-line-location-title ((t (:foreground ,zenburned-yellow
                                                     :underline t :weight bold))))
   '(w3m-history-current-url ((t (:inherit match))))
   `(w3m-lnum ((t (:foreground ,zenburned-green+2 :background ,zenburned-bg))))
   `(w3m-lnum-match ((t (:background ,zenburned-bg-1
                                     :foreground ,zenburned-orange
                                     :weight bold))))
   `(w3m-lnum-minibuffer-prompt ((t (:foreground ,zenburned-yellow))))
;;;;; erc
   `(erc-action-face ((t (:inherit erc-default-face))))
   `(erc-bold-face ((t (:weight bold))))
   `(erc-current-nick-face ((t (:foreground ,zenburned-blue :weight bold))))
   `(erc-dangerous-host-face ((t (:inherit font-lock-warning-face))))
   `(erc-default-face ((t (:foreground ,zenburned-fg))))
   `(erc-direct-msg-face ((t (:inherit erc-default-face))))
   `(erc-error-face ((t (:inherit font-lock-warning-face))))
   `(erc-fool-face ((t (:inherit erc-default-face))))
   `(erc-highlight-face ((t (:inherit hover-highlight))))
   `(erc-input-face ((t (:foreground ,zenburned-yellow))))
   `(erc-keyword-face ((t (:foreground ,zenburned-blue :weight bold))))
   `(erc-nick-default-face ((t (:foreground ,zenburned-yellow :weight bold))))
   `(erc-my-nick-face ((t (:foreground ,zenburned-red :weight bold))))
   `(erc-nick-msg-face ((t (:inherit erc-default-face))))
   `(erc-notice-face ((t (:foreground ,zenburned-green))))
   `(erc-pal-face ((t (:foreground ,zenburned-orange :weight bold))))
   `(erc-prompt-face ((t (:foreground ,zenburned-orange :background ,zenburned-bg :weight bold))))
   `(erc-timestamp-face ((t (:foreground ,zenburned-green+4))))
   `(erc-underline-face ((t (:underline t))))
;;;;; eros
   `(eros-result-overlay-face ((t (:background unspecified))))
;;;;; ert
   `(ert-test-result-expected ((t (:foreground ,zenburned-green+4 :background ,zenburned-bg))))
   `(ert-test-result-unexpected ((t (:foreground ,zenburned-red :background ,zenburned-bg))))
;;;;; eshell
   `(eshell-prompt ((t (:foreground ,zenburned-yellow :weight bold))))
   `(eshell-ls-archive ((t (:foreground ,zenburned-red-1 :weight bold))))
   `(eshell-ls-backup ((t (:inherit font-lock-comment-face))))
   `(eshell-ls-clutter ((t (:inherit font-lock-comment-face))))
   `(eshell-ls-directory ((t (:foreground ,zenburned-blue+1 :weight bold))))
   `(eshell-ls-executable ((t (:foreground ,zenburned-red+1 :weight bold))))
   `(eshell-ls-unreadable ((t (:foreground ,zenburned-fg))))
   `(eshell-ls-missing ((t (:inherit font-lock-warning-face))))
   `(eshell-ls-product ((t (:inherit font-lock-doc-face))))
   `(eshell-ls-special ((t (:foreground ,zenburned-yellow :weight bold))))
   `(eshell-ls-symlink ((t (:foreground ,zenburned-cyan :weight bold))))
;;;;; flx
   `(flx-highlight-face ((t (:foreground ,zenburned-green+2 :weight bold))))
;;;;; flycheck
   `(flycheck-error
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,zenburned-red-1) :inherit unspecified))
      (t (:foreground ,zenburned-red-1 :weight bold :underline t))))
   `(flycheck-warning
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,zenburned-yellow) :inherit unspecified))
      (t (:foreground ,zenburned-yellow :weight bold :underline t))))
   `(flycheck-info
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,zenburned-cyan) :inherit unspecified))
      (t (:foreground ,zenburned-cyan :weight bold :underline t))))
   `(flycheck-fringe-error ((t (:foreground ,zenburned-red-1 :weight bold))))
   `(flycheck-fringe-warning ((t (:foreground ,zenburned-yellow :weight bold))))
   `(flycheck-fringe-info ((t (:foreground ,zenburned-cyan :weight bold))))
;;;;; flymake
   `(flymake-errline
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,zenburned-red)
                   :inherit unspecified :foreground unspecified :background unspecified))
      (t (:foreground ,zenburned-red-1 :weight bold :underline t))))
   `(flymake-warnline
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,zenburned-orange)
                   :inherit unspecified :foreground unspecified :background unspecified))
      (t (:foreground ,zenburned-orange :weight bold :underline t))))
   `(flymake-infoline
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,zenburned-green)
                   :inherit unspecified :foreground unspecified :background unspecified))
      (t (:foreground ,zenburned-green-2 :weight bold :underline t))))
   `(flymake-error
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,zenburned-red)
                   :inherit unspecified :foreground unspecified :background unspecified))
      (t (:foreground ,zenburned-red-1 :weight bold :underline t))))
   `(flymake-warning
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,zenburned-orange)
                   :inherit unspecified :foreground unspecified :background unspecified))
      (t (:foreground ,zenburned-orange :weight bold :underline t))))
   `(flymake-note
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,zenburned-green)
                   :inherit unspecified :foreground unspecified :background unspecified))
      (t (:foreground ,zenburned-green-2 :weight bold :underline t))))
;;;;; flyspell
   `(flyspell-duplicate
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,zenburned-orange) :inherit unspecified))
      (t (:foreground ,zenburned-orange :weight bold :underline t))))
   `(flyspell-incorrect
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,zenburned-red) :inherit unspecified))
      (t (:foreground ,zenburned-red-1 :weight bold :underline t))))
;;;;; full-ack
   `(ack-separator ((t (:foreground ,zenburned-fg))))
   `(ack-file ((t (:foreground ,zenburned-blue))))
   `(ack-line ((t (:foreground ,zenburned-yellow))))
   `(ack-match ((t (:foreground ,zenburned-orange :background ,zenburned-bg-1 :weight bold))))
;;;;; git-annex
   '(git-annex-dired-annexed-available ((t (:inherit success :weight normal))))
   '(git-annex-dired-annexed-unavailable ((t (:inherit error :weight normal))))
;;;;; git-commit
   `(git-commit-comment-action  ((,class (:foreground ,zenburned-green+1 :weight bold))))
   `(git-commit-comment-branch  ((,class (:foreground ,zenburned-blue+1  :weight bold)))) ; obsolete
   `(git-commit-comment-branch-local  ((,class (:foreground ,zenburned-blue+1  :weight bold))))
   `(git-commit-comment-branch-remote ((,class (:foreground ,zenburned-green  :weight bold))))
   `(git-commit-comment-heading ((,class (:foreground ,zenburned-yellow  :weight bold))))
;;;;; git-gutter
   `(git-gutter:added ((t (:foreground ,zenburned-green :weight bold :inverse-video t))))
   `(git-gutter:deleted ((t (:foreground ,zenburned-red :weight bold :inverse-video t))))
   `(git-gutter:modified ((t (:foreground ,zenburned-magenta :weight bold :inverse-video t))))
   `(git-gutter:unchanged ((t (:foreground ,zenburned-fg :weight bold :inverse-video t))))
;;;;; git-gutter-fr
   `(git-gutter-fr:added ((t (:foreground ,zenburned-green  :weight bold))))
   `(git-gutter-fr:deleted ((t (:foreground ,zenburned-red :weight bold))))
   `(git-gutter-fr:modified ((t (:foreground ,zenburned-magenta :weight bold))))
;;;;; git-rebase
   `(git-rebase-hash ((t (:foreground, zenburned-orange))))
;;;;; gnus
   `(gnus-group-mail-1 ((t (:weight bold :inherit gnus-group-mail-1-empty))))
   `(gnus-group-mail-1-empty ((t (:inherit gnus-group-news-1-empty))))
   `(gnus-group-mail-2 ((t (:weight bold :inherit gnus-group-mail-2-empty))))
   `(gnus-group-mail-2-empty ((t (:inherit gnus-group-news-2-empty))))
   `(gnus-group-mail-3 ((t (:weight bold :inherit gnus-group-mail-3-empty))))
   `(gnus-group-mail-3-empty ((t (:inherit gnus-group-news-3-empty))))
   `(gnus-group-mail-4 ((t (:weight bold :inherit gnus-group-mail-4-empty))))
   `(gnus-group-mail-4-empty ((t (:inherit gnus-group-news-4-empty))))
   `(gnus-group-mail-5 ((t (:weight bold :inherit gnus-group-mail-5-empty))))
   `(gnus-group-mail-5-empty ((t (:inherit gnus-group-news-5-empty))))
   `(gnus-group-mail-6 ((t (:weight bold :inherit gnus-group-mail-6-empty))))
   `(gnus-group-mail-6-empty ((t (:inherit gnus-group-news-6-empty))))
   `(gnus-group-mail-low ((t (:weight bold :inherit gnus-group-mail-low-empty))))
   `(gnus-group-mail-low-empty ((t (:inherit gnus-group-news-low-empty))))
   `(gnus-group-news-1 ((t (:weight bold :inherit gnus-group-news-1-empty))))
   `(gnus-group-news-2 ((t (:weight bold :inherit gnus-group-news-2-empty))))
   `(gnus-group-news-3 ((t (:weight bold :inherit gnus-group-news-3-empty))))
   `(gnus-group-news-4 ((t (:weight bold :inherit gnus-group-news-4-empty))))
   `(gnus-group-news-5 ((t (:weight bold :inherit gnus-group-news-5-empty))))
   `(gnus-group-news-6 ((t (:weight bold :inherit gnus-group-news-6-empty))))
   `(gnus-group-news-low ((t (:weight bold :inherit gnus-group-news-low-empty))))
   `(gnus-header-content ((t (:inherit message-header-other))))
   `(gnus-header-from ((t (:inherit message-header-to))))
   `(gnus-header-name ((t (:inherit message-header-name))))
   `(gnus-header-newsgroups ((t (:inherit message-header-other))))
   `(gnus-header-subject ((t (:inherit message-header-subject))))
   `(gnus-server-opened ((t (:foreground ,zenburned-green+2 :weight bold))))
   `(gnus-server-denied ((t (:foreground ,zenburned-red+1 :weight bold))))
   `(gnus-server-closed ((t (:foreground ,zenburned-blue :slant italic))))
   `(gnus-server-offline ((t (:foreground ,zenburned-yellow :weight bold))))
   `(gnus-server-agent ((t (:foreground ,zenburned-blue :weight bold))))
   `(gnus-summary-cancelled ((t (:foreground ,zenburned-orange))))
   `(gnus-summary-high-ancient ((t (:foreground ,zenburned-blue))))
   `(gnus-summary-high-read ((t (:foreground ,zenburned-green :weight bold))))
   `(gnus-summary-high-ticked ((t (:foreground ,zenburned-orange :weight bold))))
   `(gnus-summary-high-unread ((t (:foreground ,zenburned-fg :weight bold))))
   `(gnus-summary-low-ancient ((t (:foreground ,zenburned-blue))))
   `(gnus-summary-low-read ((t (:foreground ,zenburned-green))))
   `(gnus-summary-low-ticked ((t (:foreground ,zenburned-orange :weight bold))))
   `(gnus-summary-low-unread ((t (:foreground ,zenburned-fg))))
   `(gnus-summary-normal-ancient ((t (:foreground ,zenburned-blue))))
   `(gnus-summary-normal-read ((t (:foreground ,zenburned-green))))
   `(gnus-summary-normal-ticked ((t (:foreground ,zenburned-orange :weight bold))))
   `(gnus-summary-normal-unread ((t (:foreground ,zenburned-fg))))
   `(gnus-summary-selected ((t (:foreground ,zenburned-yellow :weight bold))))
   `(gnus-cite-1 ((t (:foreground ,zenburned-blue))))
   `(gnus-cite-10 ((t (:foreground ,zenburned-yellow-1))))
   `(gnus-cite-11 ((t (:foreground ,zenburned-yellow))))
   `(gnus-cite-2 ((t (:foreground ,zenburned-blue-1))))
   `(gnus-cite-3 ((t (:foreground ,zenburned-blue-2))))
   `(gnus-cite-4 ((t (:foreground ,zenburned-green+2))))
   `(gnus-cite-5 ((t (:foreground ,zenburned-green+1))))
   `(gnus-cite-6 ((t (:foreground ,zenburned-green))))
   `(gnus-cite-7 ((t (:foreground ,zenburned-red))))
   `(gnus-cite-8 ((t (:foreground ,zenburned-red-1))))
   `(gnus-cite-9 ((t (:foreground ,zenburned-red-2))))
   `(gnus-group-news-1-empty ((t (:foreground ,zenburned-yellow))))
   `(gnus-group-news-2-empty ((t (:foreground ,zenburned-green+3))))
   `(gnus-group-news-3-empty ((t (:foreground ,zenburned-green+1))))
   `(gnus-group-news-4-empty ((t (:foreground ,zenburned-blue-2))))
   `(gnus-group-news-5-empty ((t (:foreground ,zenburned-blue-3))))
   `(gnus-group-news-6-empty ((t (:foreground ,zenburned-bg+2))))
   `(gnus-group-news-low-empty ((t (:foreground ,zenburned-bg+2))))
   `(gnus-signature ((t (:foreground ,zenburned-yellow))))
   `(gnus-x ((t (:background ,zenburned-fg :foreground ,zenburned-bg))))
   `(mm-uu-extract ((t (:background ,zenburned-bg-05 :foreground ,zenburned-green+1))))
;;;;; go-guru
   `(go-guru-hl-identifier-face ((t (:foreground ,zenburned-bg-1 :background ,zenburned-green+1))))
;;;;; guide-key
   `(guide-key/highlight-command-face ((t (:foreground ,zenburned-blue))))
   `(guide-key/key-face ((t (:foreground ,zenburned-green))))
   `(guide-key/prefix-command-face ((t (:foreground ,zenburned-green+1))))
;;;;; hackernews
   '(hackernews-comment-count ((t (:inherit link-visited :underline nil))))
   '(hackernews-link          ((t (:inherit link         :underline nil))))
;;;;; helm
   `(helm-header
     ((t (:foreground ,zenburned-green
                      :background ,zenburned-bg
                      :underline nil
                      :box nil
                      :extend t))))
   `(helm-source-header
     ((t (:foreground ,zenburned-yellow
                      :background ,zenburned-bg-1
                      :underline nil
                      :weight bold
                      :box (:line-width -1 :style released-button)
                      :extend t))))
   `(helm-selection ((t (:background ,zenburned-bg+1 :underline nil))))
   `(helm-selection-line ((t (:background ,zenburned-bg+1))))
   `(helm-visible-mark ((t (:foreground ,zenburned-bg :background ,zenburned-yellow-2))))
   `(helm-candidate-number ((t (:foreground ,zenburned-green+4 :background ,zenburned-bg-1))))
   `(helm-separator ((t (:foreground ,zenburned-red :background ,zenburned-bg))))
   `(helm-time-zone-current ((t (:foreground ,zenburned-green+2 :background ,zenburned-bg))))
   `(helm-time-zone-home ((t (:foreground ,zenburned-red :background ,zenburned-bg))))
   `(helm-bookmark-addressbook ((t (:foreground ,zenburned-orange :background ,zenburned-bg))))
   `(helm-bookmark-directory ((t (:foreground nil :background nil :inherit helm-ff-directory))))
   `(helm-bookmark-file ((t (:foreground nil :background nil :inherit helm-ff-file))))
   `(helm-bookmark-gnus ((t (:foreground ,zenburned-magenta :background ,zenburned-bg))))
   `(helm-bookmark-info ((t (:foreground ,zenburned-green+2 :background ,zenburned-bg))))
   `(helm-bookmark-man ((t (:foreground ,zenburned-yellow :background ,zenburned-bg))))
   `(helm-bookmark-w3m ((t (:foreground ,zenburned-magenta :background ,zenburned-bg))))
   `(helm-buffer-not-saved ((t (:foreground ,zenburned-red :background ,zenburned-bg))))
   `(helm-buffer-process ((t (:foreground ,zenburned-cyan :background ,zenburned-bg))))
   `(helm-buffer-saved-out ((t (:foreground ,zenburned-fg :background ,zenburned-bg))))
   `(helm-buffer-size ((t (:foreground ,zenburned-fg-1 :background ,zenburned-bg))))
   `(helm-ff-directory ((t (:foreground ,zenburned-cyan :background ,zenburned-bg :weight bold))))
   `(helm-ff-file ((t (:foreground ,zenburned-fg :background ,zenburned-bg :weight normal))))
   `(helm-ff-file-extension ((t (:foreground ,zenburned-fg :background ,zenburned-bg :weight normal))))
   `(helm-ff-executable ((t (:foreground ,zenburned-green+2 :background ,zenburned-bg :weight normal))))
   `(helm-ff-invalid-symlink ((t (:foreground ,zenburned-red :background ,zenburned-bg :weight bold))))
   `(helm-ff-symlink ((t (:foreground ,zenburned-yellow :background ,zenburned-bg :weight bold))))
   `(helm-ff-prefix ((t (:foreground ,zenburned-bg :background ,zenburned-yellow :weight normal))))
   `(helm-grep-cmd-line ((t (:foreground ,zenburned-cyan :background ,zenburned-bg))))
   `(helm-grep-file ((t (:foreground ,zenburned-fg :background ,zenburned-bg))))
   `(helm-grep-finish ((t (:foreground ,zenburned-green+2 :background ,zenburned-bg))))
   `(helm-grep-lineno ((t (:foreground ,zenburned-fg-1 :background ,zenburned-bg))))
   `(helm-grep-match ((t (:foreground nil :background nil :inherit helm-match))))
   `(helm-grep-running ((t (:foreground ,zenburned-red :background ,zenburned-bg))))
   `(helm-match ((t (:foreground ,zenburned-orange :background ,zenburned-bg-1 :weight bold))))
   `(helm-moccur-buffer ((t (:foreground ,zenburned-cyan :background ,zenburned-bg))))
   `(helm-mu-contacts-address-face ((t (:foreground ,zenburned-fg-1 :background ,zenburned-bg))))
   `(helm-mu-contacts-name-face ((t (:foreground ,zenburned-fg :background ,zenburned-bg))))
;;;;; helm-lxc
   `(helm-lxc-face-frozen ((t (:foreground ,zenburned-blue :background ,zenburned-bg))))
   `(helm-lxc-face-running ((t (:foreground ,zenburned-green :background ,zenburned-bg))))
   `(helm-lxc-face-stopped ((t (:foreground ,zenburned-red :background ,zenburned-bg))))
;;;;; helm-swoop
   `(helm-swoop-target-line-face ((t (:foreground ,zenburned-fg :background ,zenburned-bg+1))))
   `(helm-swoop-target-word-face ((t (:foreground ,zenburned-yellow :background ,zenburned-bg+2 :weight bold))))
;;;;; highlight-numbers
   `(highlight-numbers-number ((t (:foreground ,zenburned-blue))))
;;;;; highlight-symbol
   `(highlight-symbol-face ((t (:background ,zenburned-bg+2))))
;;;;; highlight-thing
   `(highlight-thing ((t (:background ,zenburned-bg+2))))
;;;;; hl-line-mode
   `(hl-line-face ((,class (:background ,zenburned-bg-05))
                   (t :weight bold)))
   `(hl-line ((,class (:background ,zenburned-bg-05 :extend t)) ; old emacsen
              (t :weight bold)))
;;;;; hl-sexp
   `(hl-sexp-face ((,class (:background ,zenburned-bg+1))
                   (t :weight bold)))
;;;;; hydra
   `(hydra-face-red ((t (:foreground ,zenburned-red-1 :background ,zenburned-bg))))
   `(hydra-face-amaranth ((t (:foreground ,zenburned-red-3 :background ,zenburned-bg))))
   `(hydra-face-blue ((t (:foreground ,zenburned-blue :background ,zenburned-bg))))
   `(hydra-face-pink ((t (:foreground ,zenburned-magenta :background ,zenburned-bg))))
   `(hydra-face-teal ((t (:foreground ,zenburned-cyan :background ,zenburned-bg))))
;;;;; info+
   `(info-command-ref-item ((t (:background ,zenburned-bg-1 :foreground ,zenburned-orange))))
   `(info-constant-ref-item ((t (:background ,zenburned-bg-1 :foreground ,zenburned-magenta))))
   `(info-double-quoted-name ((t (:inherit font-lock-comment-face))))
   `(info-file ((t (:background ,zenburned-bg-1 :foreground ,zenburned-yellow))))
   `(info-function-ref-item ((t (:background ,zenburned-bg-1 :inherit font-lock-function-name-face))))
   `(info-macro-ref-item ((t (:background ,zenburned-bg-1 :foreground ,zenburned-yellow))))
   `(info-menu ((t (:foreground ,zenburned-yellow))))
   `(info-quoted-name ((t (:inherit font-lock-constant-face))))
   `(info-reference-item ((t (:background ,zenburned-bg-1))))
   `(info-single-quote ((t (:inherit font-lock-keyword-face))))
   `(info-special-form-ref-item ((t (:background ,zenburned-bg-1 :foreground ,zenburned-yellow))))
   `(info-string ((t (:inherit font-lock-string-face))))
   `(info-syntax-class-item ((t (:background ,zenburned-bg-1 :foreground ,zenburned-blue+1))))
   `(info-user-option-ref-item ((t (:background ,zenburned-bg-1 :foreground ,zenburned-red))))
   `(info-variable-ref-item ((t (:background ,zenburned-bg-1 :foreground ,zenburned-orange))))
;;;;; irfc
   `(irfc-head-name-face ((t (:foreground ,zenburned-red :weight bold))))
   `(irfc-head-number-face ((t (:foreground ,zenburned-red :weight bold))))
   `(irfc-reference-face ((t (:foreground ,zenburned-blue-1 :weight bold))))
   `(irfc-requirement-keyword-face ((t (:inherit font-lock-keyword-face))))
   `(irfc-rfc-link-face ((t (:inherit link))))
   `(irfc-rfc-number-face ((t (:foreground ,zenburned-cyan :weight bold))))
   `(irfc-std-number-face ((t (:foreground ,zenburned-green+4 :weight bold))))
   `(irfc-table-item-face ((t (:foreground ,zenburned-green+3))))
   `(irfc-title-face ((t (:foreground ,zenburned-yellow
                                      :underline t :weight bold))))
;;;;; ivy
   `(ivy-confirm-face ((t (:foreground ,zenburned-green :background ,zenburned-bg))))
   `(ivy-current-match ((t (:foreground ,zenburned-yellow :weight bold :underline t))))
   `(ivy-cursor ((t (:foreground ,zenburned-bg :background ,zenburned-fg))))
   `(ivy-match-required-face ((t (:foreground ,zenburned-red :background ,zenburned-bg))))
   `(ivy-minibuffer-match-face-1 ((t (:background ,zenburned-bg+1))))
   `(ivy-minibuffer-match-face-2 ((t (:background ,zenburned-green-2))))
   `(ivy-minibuffer-match-face-3 ((t (:background ,zenburned-green))))
   `(ivy-minibuffer-match-face-4 ((t (:background ,zenburned-green+1))))
   `(ivy-remote ((t (:foreground ,zenburned-blue :background ,zenburned-bg))))
   `(ivy-subdir ((t (:foreground ,zenburned-yellow :background ,zenburned-bg))))
;;;;; ido-mode
   `(ido-first-match ((t (:foreground ,zenburned-yellow :weight bold))))
   `(ido-only-match ((t (:foreground ,zenburned-orange :weight bold))))
   `(ido-subdir ((t (:foreground ,zenburned-yellow))))
   `(ido-indicator ((t (:foreground ,zenburned-yellow :background ,zenburned-red-4))))
;;;;; iedit-mode
   `(iedit-occurrence ((t (:background ,zenburned-bg+2 :weight bold))))
;;;;; jabber-mode
   `(jabber-roster-user-away ((t (:foreground ,zenburned-green+2))))
   `(jabber-roster-user-online ((t (:foreground ,zenburned-blue-1))))
   `(jabber-roster-user-dnd ((t (:foreground ,zenburned-red+1))))
   `(jabber-roster-user-xa ((t (:foreground ,zenburned-magenta))))
   `(jabber-roster-user-chatty ((t (:foreground ,zenburned-orange))))
   `(jabber-roster-user-error ((t (:foreground ,zenburned-red+1))))
   `(jabber-rare-time-face ((t (:foreground ,zenburned-green+1))))
   `(jabber-chat-prompt-local ((t (:foreground ,zenburned-blue-1))))
   `(jabber-chat-prompt-foreign ((t (:foreground ,zenburned-red+1))))
   `(jabber-chat-prompt-system ((t (:foreground ,zenburned-green+3))))
   `(jabber-activity-face((t (:foreground ,zenburned-red+1))))
   `(jabber-activity-personal-face ((t (:foreground ,zenburned-blue+1))))
   `(jabber-title-small ((t (:height 1.1 :weight bold))))
   `(jabber-title-medium ((t (:height 1.2 :weight bold))))
   `(jabber-title-large ((t (:height 1.3 :weight bold))))
;;;;; js2-mode
   `(js2-warning ((t (:underline ,zenburned-orange))))
   `(js2-error ((t (:foreground ,zenburned-red :weight bold))))
   `(js2-jsdoc-tag ((t (:foreground ,zenburned-green-2))))
   `(js2-jsdoc-type ((t (:foreground ,zenburned-green+2))))
   `(js2-jsdoc-value ((t (:foreground ,zenburned-green+3))))
   `(js2-function-param ((t (:foreground, zenburned-orange))))
   `(js2-external-variable ((t (:foreground ,zenburned-orange))))
;;;;; additional js2 mode attributes for better syntax highlighting
   `(js2-instance-member ((t (:foreground ,zenburned-green-2))))
   `(js2-jsdoc-html-tag-delimiter ((t (:foreground ,zenburned-orange))))
   `(js2-jsdoc-html-tag-name ((t (:foreground ,zenburned-red-1))))
   `(js2-object-property ((t (:foreground ,zenburned-blue+1))))
   `(js2-magic-paren ((t (:foreground ,zenburned-blue-5))))
   `(js2-private-function-call ((t (:foreground ,zenburned-cyan))))
   `(js2-function-call ((t (:foreground ,zenburned-cyan))))
   `(js2-private-member ((t (:foreground ,zenburned-blue-1))))
   `(js2-keywords ((t (:foreground ,zenburned-magenta))))
;;;;; ledger-mode
   `(ledger-font-payee-uncleared-face ((t (:foreground ,zenburned-red-1 :weight bold))))
   `(ledger-font-payee-cleared-face ((t (:foreground ,zenburned-fg :weight normal))))
   `(ledger-font-payee-pending-face ((t (:foreground ,zenburned-red :weight normal))))
   `(ledger-font-xact-highlight-face ((t (:background ,zenburned-bg+1))))
   `(ledger-font-auto-xact-face ((t (:foreground ,zenburned-yellow-1 :weight normal))))
   `(ledger-font-periodic-xact-face ((t (:foreground ,zenburned-green :weight normal))))
   `(ledger-font-pending-face ((t (:foreground ,zenburned-orange weight: normal))))
   `(ledger-font-other-face ((t (:foreground ,zenburned-fg))))
   `(ledger-font-posting-date-face ((t (:foreground ,zenburned-orange :weight normal))))
   `(ledger-font-posting-account-face ((t (:foreground ,zenburned-blue-1))))
   `(ledger-font-posting-account-cleared-face ((t (:foreground ,zenburned-fg))))
   `(ledger-font-posting-account-pending-face ((t (:foreground ,zenburned-orange))))
   `(ledger-font-posting-amount-face ((t (:foreground ,zenburned-orange))))
   `(ledger-occur-narrowed-face ((t (:foreground ,zenburned-fg-1 :invisible t))))
   `(ledger-occur-xact-face ((t (:background ,zenburned-bg+1))))
   `(ledger-font-comment-face ((t (:foreground ,zenburned-green))))
   `(ledger-font-reconciler-uncleared-face ((t (:foreground ,zenburned-red-1 :weight bold))))
   `(ledger-font-reconciler-cleared-face ((t (:foreground ,zenburned-fg :weight normal))))
   `(ledger-font-reconciler-pending-face ((t (:foreground ,zenburned-orange :weight normal))))
   `(ledger-font-report-clickable-face ((t (:foreground ,zenburned-orange :weight normal))))
;;;;; linum-mode
   `(linum ((t (:foreground ,zenburned-green+2 :background ,zenburned-bg))))
;;;;; lispy
   `(lispy-command-name-face ((t (:background ,zenburned-bg-05 :inherit font-lock-function-name-face))))
   `(lispy-cursor-face ((t (:foreground ,zenburned-bg :background ,zenburned-fg))))
   `(lispy-face-hint ((t (:inherit highlight :foreground ,zenburned-yellow))))
;;;;; ruler-mode
   `(ruler-mode-column-number ((t (:inherit 'ruler-mode-default :foreground ,zenburned-fg))))
   `(ruler-mode-fill-column ((t (:inherit 'ruler-mode-default :foreground ,zenburned-yellow))))
   `(ruler-mode-goal-column ((t (:inherit 'ruler-mode-fill-column))))
   `(ruler-mode-comment-column ((t (:inherit 'ruler-mode-fill-column))))
   `(ruler-mode-tab-stop ((t (:inherit 'ruler-mode-fill-column))))
   `(ruler-mode-current-column ((t (:foreground ,zenburned-yellow :box t))))
   `(ruler-mode-default ((t (:foreground ,zenburned-green+2 :background ,zenburned-bg))))

;;;;; lui
   `(lui-time-stamp-face ((t (:foreground ,zenburned-blue-1))))
   `(lui-hilight-face ((t (:foreground ,zenburned-green+2 :background ,zenburned-bg))))
   `(lui-button-face ((t (:inherit hover-highlight))))
;;;;; macrostep
   `(macrostep-gensym-1
     ((t (:foreground ,zenburned-green+2 :background ,zenburned-bg-1))))
   `(macrostep-gensym-2
     ((t (:foreground ,zenburned-red+1 :background ,zenburned-bg-1))))
   `(macrostep-gensym-3
     ((t (:foreground ,zenburned-blue+1 :background ,zenburned-bg-1))))
   `(macrostep-gensym-4
     ((t (:foreground ,zenburned-magenta :background ,zenburned-bg-1))))
   `(macrostep-gensym-5
     ((t (:foreground ,zenburned-yellow :background ,zenburned-bg-1))))
   `(macrostep-expansion-highlight-face
     ((t (:inherit highlight))))
   `(macrostep-macro-face
     ((t (:underline t))))
;;;;; magit
;;;;;; headings and diffs
   ;; Please read (info "(magit)Theming Faces") before changing this.
   `(magit-section-highlight           ((t (:background ,zenburned-bg+05))))
   `(magit-section-heading             ((t (:foreground ,zenburned-yellow :weight bold))))
   `(magit-section-heading-selection   ((t (:foreground ,zenburned-orange :weight bold))))
   `(magit-diff-file-heading           ((t (:weight bold))))
   `(magit-diff-file-heading-highlight ((t (:background ,zenburned-bg+05 :weight bold))))
   `(magit-diff-file-heading-selection ((t (:background ,zenburned-bg+05 :weight bold
                                                        :foreground ,zenburned-orange))))
   `(magit-diff-added                  ((t (:background ,zenburned-green-2))))
   `(magit-diff-added-highlight        ((t (:background ,zenburned-green))))
   `(magit-diff-removed                ((t (:background ,zenburned-red-4))))
   `(magit-diff-removed-highlight      ((t (:background ,zenburned-red-3))))
   `(magit-diff-hunk-heading           ((t (:background ,zenburned-bg+1))))
   `(magit-diff-hunk-heading-highlight ((t (:background ,zenburned-bg+2))))
   `(magit-diff-hunk-heading-selection ((t (:background ,zenburned-bg+2
                                                        :foreground ,zenburned-orange))))
   `(magit-diff-lines-heading          ((t (:background ,zenburned-orange
                                                        :foreground ,zenburned-bg+2))))
   `(magit-diff-context-highlight      ((t (:background ,zenburned-bg+05
                                                        :foreground "grey70"))))
   `(magit-diffstat-added              ((t (:foreground ,zenburned-green+4))))
   `(magit-diffstat-removed            ((t (:foreground ,zenburned-red))))
;;;;;; popup
   `(magit-popup-heading             ((t (:foreground ,zenburned-yellow  :weight bold))))
   `(magit-popup-key                 ((t (:foreground ,zenburned-green-2 :weight bold))))
   `(magit-popup-argument            ((t (:foreground ,zenburned-green   :weight bold))))
   `(magit-popup-disabled-argument   ((t (:foreground ,zenburned-fg-1    :weight normal))))
   `(magit-popup-option-value        ((t (:foreground ,zenburned-blue-2  :weight bold))))
;;;;;; process
   `(magit-process-ok    ((t (:foreground ,zenburned-green  :weight bold))))
   `(magit-process-ng    ((t (:foreground ,zenburned-red    :weight bold))))
;;;;;; log
   `(magit-log-author    ((t (:foreground ,zenburned-orange))))
   `(magit-log-date      ((t (:foreground ,zenburned-fg-1))))
   `(magit-log-graph     ((t (:foreground ,zenburned-fg+1))))
;;;;;; sequence
   `(magit-sequence-pick ((t (:foreground ,zenburned-yellow-2))))
   `(magit-sequence-stop ((t (:foreground ,zenburned-green))))
   `(magit-sequence-part ((t (:foreground ,zenburned-yellow))))
   `(magit-sequence-head ((t (:foreground ,zenburned-blue))))
   `(magit-sequence-drop ((t (:foreground ,zenburned-red))))
   `(magit-sequence-done ((t (:foreground ,zenburned-fg-1))))
   `(magit-sequence-onto ((t (:foreground ,zenburned-fg-1))))
;;;;;; bisect
   `(magit-bisect-good ((t (:foreground ,zenburned-green))))
   `(magit-bisect-skip ((t (:foreground ,zenburned-yellow))))
   `(magit-bisect-bad  ((t (:foreground ,zenburned-red))))
;;;;;; blame
   `(magit-blame-heading ((t (:background ,zenburned-bg-1 :foreground ,zenburned-blue-2))))
   `(magit-blame-hash    ((t (:background ,zenburned-bg-1 :foreground ,zenburned-blue-2))))
   `(magit-blame-name    ((t (:background ,zenburned-bg-1 :foreground ,zenburned-orange))))
   `(magit-blame-date    ((t (:background ,zenburned-bg-1 :foreground ,zenburned-orange))))
   `(magit-blame-summary ((t (:background ,zenburned-bg-1 :foreground ,zenburned-blue-2
                                          :weight bold))))
;;;;;; references etc
   `(magit-dimmed         ((t (:foreground ,zenburned-bg+3))))
   `(magit-hash           ((t (:foreground ,zenburned-bg+3))))
   `(magit-tag            ((t (:foreground ,zenburned-orange :weight bold))))
   `(magit-branch-remote  ((t (:foreground ,zenburned-green  :weight bold))))
   `(magit-branch-local   ((t (:foreground ,zenburned-blue   :weight bold))))
   `(magit-branch-current ((t (:foreground ,zenburned-blue   :weight bold :box t))))
   `(magit-head           ((t (:foreground ,zenburned-blue   :weight bold))))
   `(magit-refname        ((t (:background ,zenburned-bg+2 :foreground ,zenburned-fg :weight bold))))
   `(magit-refname-stash  ((t (:background ,zenburned-bg+2 :foreground ,zenburned-fg :weight bold))))
   `(magit-refname-wip    ((t (:background ,zenburned-bg+2 :foreground ,zenburned-fg :weight bold))))
   `(magit-signature-good      ((t (:foreground ,zenburned-green))))
   `(magit-signature-bad       ((t (:foreground ,zenburned-red))))
   `(magit-signature-untrusted ((t (:foreground ,zenburned-yellow))))
   `(magit-signature-expired   ((t (:foreground ,zenburned-orange))))
   `(magit-signature-revoked   ((t (:foreground ,zenburned-magenta))))
   '(magit-signature-error     ((t (:inherit    magit-signature-bad))))
   `(magit-cherry-unmatched    ((t (:foreground ,zenburned-cyan))))
   `(magit-cherry-equivalent   ((t (:foreground ,zenburned-magenta))))
   `(magit-reflog-commit       ((t (:foreground ,zenburned-green))))
   `(magit-reflog-amend        ((t (:foreground ,zenburned-magenta))))
   `(magit-reflog-merge        ((t (:foreground ,zenburned-green))))
   `(magit-reflog-checkout     ((t (:foreground ,zenburned-blue))))
   `(magit-reflog-reset        ((t (:foreground ,zenburned-red))))
   `(magit-reflog-rebase       ((t (:foreground ,zenburned-magenta))))
   `(magit-reflog-cherry-pick  ((t (:foreground ,zenburned-green))))
   `(magit-reflog-remote       ((t (:foreground ,zenburned-cyan))))
   `(magit-reflog-other        ((t (:foreground ,zenburned-cyan))))
;;;;; markup-faces
   `(markup-anchor-face ((t (:foreground ,zenburned-blue+1))))
   `(markup-code-face ((t (:inherit font-lock-constant-face))))
   `(markup-command-face ((t (:foreground ,zenburned-yellow))))
   `(markup-emphasis-face ((t (:inherit bold))))
   `(markup-internal-reference-face ((t (:foreground ,zenburned-yellow-2 :underline t))))
   `(markup-list-face ((t (:foreground ,zenburned-fg+1))))
   `(markup-meta-face ((t (:foreground ,zenburned-yellow))))
   `(markup-meta-hide-face ((t (:foreground ,zenburned-yellow))))
   `(markup-secondary-text-face ((t (:foreground ,zenburned-yellow-1))))
   `(markup-title-0-face ((t (:inherit font-lock-function-name-face :weight bold))))
   `(markup-title-1-face ((t (:inherit font-lock-function-name-face :weight bold))))
   `(markup-title-2-face ((t (:inherit font-lock-function-name-face :weight bold))))
   `(markup-title-3-face ((t (:inherit font-lock-function-name-face :weight bold))))
   `(markup-title-4-face ((t (:inherit font-lock-function-name-face :weight bold))))
   `(markup-typewriter-face ((t (:inherit font-lock-constant-face))))
   `(markup-verbatim-face ((t (:inherit font-lock-constant-face))))
   `(markup-value-face ((t (:foreground ,zenburned-yellow))))
;;;;; message-mode
   `(message-cited-text ((t (:inherit font-lock-comment-face))))
   `(message-header-name ((t (:foreground ,zenburned-green+1))))
   `(message-header-other ((t (:foreground ,zenburned-green))))
   `(message-header-to ((t (:foreground ,zenburned-yellow :weight bold))))
   `(message-header-cc ((t (:foreground ,zenburned-yellow :weight bold))))
   `(message-header-newsgroups ((t (:foreground ,zenburned-yellow :weight bold))))
   `(message-header-subject ((t (:foreground ,zenburned-orange :weight bold))))
   `(message-header-xheader ((t (:foreground ,zenburned-green))))
   `(message-mml ((t (:foreground ,zenburned-yellow :weight bold))))
   `(message-separator ((t (:inherit font-lock-comment-face))))
;;;;; mew
   `(mew-face-header-subject ((t (:foreground ,zenburned-orange))))
   `(mew-face-header-from ((t (:foreground ,zenburned-yellow))))
   `(mew-face-header-date ((t (:foreground ,zenburned-green))))
   `(mew-face-header-to ((t (:foreground ,zenburned-red))))
   `(mew-face-header-key ((t (:foreground ,zenburned-green))))
   `(mew-face-header-private ((t (:foreground ,zenburned-green))))
   `(mew-face-header-important ((t (:foreground ,zenburned-blue))))
   `(mew-face-header-marginal ((t (:foreground ,zenburned-fg :weight bold))))
   `(mew-face-header-warning ((t (:foreground ,zenburned-red))))
   `(mew-face-header-xmew ((t (:foreground ,zenburned-green))))
   `(mew-face-header-xmew-bad ((t (:foreground ,zenburned-red))))
   `(mew-face-body-url ((t (:foreground ,zenburned-orange))))
   `(mew-face-body-comment ((t (:foreground ,zenburned-fg :slant italic))))
   `(mew-face-body-cite1 ((t (:foreground ,zenburned-green))))
   `(mew-face-body-cite2 ((t (:foreground ,zenburned-blue))))
   `(mew-face-body-cite3 ((t (:foreground ,zenburned-orange))))
   `(mew-face-body-cite4 ((t (:foreground ,zenburned-yellow))))
   `(mew-face-body-cite5 ((t (:foreground ,zenburned-red))))
   `(mew-face-mark-review ((t (:foreground ,zenburned-blue))))
   `(mew-face-mark-escape ((t (:foreground ,zenburned-green))))
   `(mew-face-mark-delete ((t (:foreground ,zenburned-red))))
   `(mew-face-mark-unlink ((t (:foreground ,zenburned-yellow))))
   `(mew-face-mark-refile ((t (:foreground ,zenburned-green))))
   `(mew-face-mark-unread ((t (:foreground ,zenburned-red-2))))
   `(mew-face-eof-message ((t (:foreground ,zenburned-green))))
   `(mew-face-eof-part ((t (:foreground ,zenburned-yellow))))
;;;;; mic-paren
   `(paren-face-match ((t (:foreground ,zenburned-cyan :background ,zenburned-bg :weight bold))))
   `(paren-face-mismatch ((t (:foreground ,zenburned-bg :background ,zenburned-magenta :weight bold))))
   `(paren-face-no-match ((t (:foreground ,zenburned-bg :background ,zenburned-red :weight bold))))
;;;;; mingus
   `(mingus-directory-face ((t (:foreground ,zenburned-blue))))
   `(mingus-pausing-face ((t (:foreground ,zenburned-magenta))))
   `(mingus-playing-face ((t (:foreground ,zenburned-cyan))))
   `(mingus-playlist-face ((t (:foreground ,zenburned-cyan ))))
   `(mingus-mark-face ((t (:bold t :foreground ,zenburned-magenta))))
   `(mingus-song-file-face ((t (:foreground ,zenburned-yellow))))
   `(mingus-artist-face ((t (:foreground ,zenburned-cyan))))
   `(mingus-album-face ((t (:underline t :foreground ,zenburned-red+1))))
   `(mingus-album-stale-face ((t (:foreground ,zenburned-red+1))))
   `(mingus-stopped-face ((t (:foreground ,zenburned-red))))
;;;;; nav
   `(nav-face-heading ((t (:foreground ,zenburned-yellow))))
   `(nav-face-button-num ((t (:foreground ,zenburned-cyan))))
   `(nav-face-dir ((t (:foreground ,zenburned-green))))
   `(nav-face-hdir ((t (:foreground ,zenburned-red))))
   `(nav-face-file ((t (:foreground ,zenburned-fg))))
   `(nav-face-hfile ((t (:foreground ,zenburned-red-4))))
;;;;; merlin
   `(merlin-type-face ((t (:inherit highlight))))
   `(merlin-compilation-warning-face
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,zenburned-orange)))
      (t
       (:underline ,zenburned-orange))))
   `(merlin-compilation-error-face
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,zenburned-red)))
      (t
       (:underline ,zenburned-red))))
;;;;; mu4e
   `(mu4e-cited-1-face ((t (:foreground ,zenburned-blue    :slant italic))))
   `(mu4e-cited-2-face ((t (:foreground ,zenburned-green+2 :slant italic))))
   `(mu4e-cited-3-face ((t (:foreground ,zenburned-blue-2  :slant italic))))
   `(mu4e-cited-4-face ((t (:foreground ,zenburned-green   :slant italic))))
   `(mu4e-cited-5-face ((t (:foreground ,zenburned-blue-4  :slant italic))))
   `(mu4e-cited-6-face ((t (:foreground ,zenburned-green-2 :slant italic))))
   `(mu4e-cited-7-face ((t (:foreground ,zenburned-blue    :slant italic))))
   `(mu4e-replied-face ((t (:foreground ,zenburned-bg+3))))
   `(mu4e-trashed-face ((t (:foreground ,zenburned-bg+3 :strike-through t))))
;;;;; mumamo
   `(mumamo-background-chunk-major ((t (:background nil))))
   `(mumamo-background-chunk-submode1 ((t (:background ,zenburned-bg-1))))
   `(mumamo-background-chunk-submode2 ((t (:background ,zenburned-bg+2))))
   `(mumamo-background-chunk-submode3 ((t (:background ,zenburned-bg+3))))
   `(mumamo-background-chunk-submode4 ((t (:background ,zenburned-bg+1))))
;;;;; neotree
   `(neo-banner-face ((t (:foreground ,zenburned-blue+1 :weight bold))))
   `(neo-header-face ((t (:foreground ,zenburned-fg))))
   `(neo-root-dir-face ((t (:foreground ,zenburned-blue+1 :weight bold))))
   `(neo-dir-link-face ((t (:foreground ,zenburned-blue))))
   `(neo-file-link-face ((t (:foreground ,zenburned-fg))))
   `(neo-expand-btn-face ((t (:foreground ,zenburned-blue))))
   `(neo-vc-default-face ((t (:foreground ,zenburned-fg+1))))
   `(neo-vc-user-face ((t (:foreground ,zenburned-red :slant italic))))
   `(neo-vc-up-to-date-face ((t (:foreground ,zenburned-fg))))
   `(neo-vc-edited-face ((t (:foreground ,zenburned-magenta))))
   `(neo-vc-needs-merge-face ((t (:foreground ,zenburned-red+1))))
   `(neo-vc-unlocked-changes-face ((t (:foreground ,zenburned-red :background ,zenburned-blue-5))))
   `(neo-vc-added-face ((t (:foreground ,zenburned-green+1))))
   `(neo-vc-conflict-face ((t (:foreground ,zenburned-red+1))))
   `(neo-vc-missing-face ((t (:foreground ,zenburned-red+1))))
   `(neo-vc-ignored-face ((t (:foreground ,zenburned-fg-1))))
;;;;; notmuch
   `(notmuch-crypto-decryption ((t (:foreground ,zenburned-bg :background ,zenburned-magenta))))
   `(notmuch-crypto-part-header ((t (:foreground ,zenburned-blue+1))))
   `(notmuch-crypto-signature-bad ((t (:foreground ,zenburned-bg :background ,zenburned-red))))
   `(notmuch-crypto-signature-good ((t (:foreground ,zenburned-bg :background ,zenburned-green+1))))
   `(notmuch-crypto-signature-good-key ((t (:foreground ,zenburned-bg :background ,zenburned-orange))))
   `(notmuch-crypto-signature-unknown ((t (:foreground ,zenburned-bg :background ,zenburned-red))))
   `(notmuch-hello-logo-background ((t (:background ,zenburned-bg+2))))
   `(notmuch-message-summary-face ((t (:background ,zenburned-bg-08))))
   `(notmuch-search-flagged-face ((t (:foreground ,zenburned-blue+1))))
   `(notmuch-search-non-matching-authors ((t (:foreground ,zenburned-fg-1))))
   `(notmuch-tag-added ((t (:underline ,zenburned-green+1))))
   `(notmuch-tag-deleted ((t (:strike-through ,zenburned-red))))
   `(notmuch-tag-face ((t (:foreground ,zenburned-green+1))))
   `(notmuch-tag-flagged ((t (:foreground ,zenburned-blue+1))))
   `(notmuch-tag-unread ((t (:foreground ,zenburned-red))))
   `(notmuch-tree-match-author-face ((t (:foreground ,zenburned-green+1))))
   `(notmuch-tree-match-tag-face ((t (:foreground ,zenburned-green+1))))
;;;;; orderless
   `(orderless-match-face-0 ((t (:foreground ,zenburned-green))))
   `(orderless-match-face-1 ((t (:foreground ,zenburned-magenta))))
   `(orderless-match-face-2 ((t (:foreground ,zenburned-blue))))
   `(orderless-match-face-3 ((t (:foreground ,zenburned-orange))))
;;;;; org-mode
   `(org-agenda-date-today
     ((t (:foreground ,zenburned-fg+1 :slant italic :weight bold))) t)
   `(org-agenda-structure
     ((t (:inherit font-lock-comment-face))))
   `(org-archived ((t (:foreground ,zenburned-fg :weight bold))))
   `(org-block ((t (:background ,zenburned-bg+05 :extend t))))
   `(org-checkbox ((t (:background ,zenburned-bg+2 :foreground ,zenburned-fg+1
                                   :box (:line-width 1 :style released-button)))))
   `(org-date ((t (:foreground ,zenburned-blue :underline t))))
   `(org-deadline-announce ((t (:foreground ,zenburned-red-1))))
   `(org-done ((t (:weight bold :weight bold :foreground ,zenburned-green+3))))
   `(org-formula ((t (:foreground ,zenburned-yellow-2))))
   `(org-headline-done ((t (:foreground ,zenburned-green+3))))
   `(org-hide ((t (:foreground ,zenburned-bg))))
   `(org-level-1 ((t (:inherit ,z-variable-pitch :foreground ,zenburned-orange
                               ,@(when zenburned-scale-org-headlines
                                   (list :height zenburned-height-plus-4))))))
   `(org-level-2 ((t (:inherit ,z-variable-pitch :foreground ,zenburned-green+4
                               ,@(when zenburned-scale-org-headlines
                                   (list :height zenburned-height-plus-3))))))
   `(org-level-3 ((t (:inherit ,z-variable-pitch :foreground ,zenburned-blue-1
                               ,@(when zenburned-scale-org-headlines
                                   (list :height zenburned-height-plus-2))))))
   `(org-level-4 ((t (:inherit ,z-variable-pitch :foreground ,zenburned-yellow-2
                               ,@(when zenburned-scale-org-headlines
                                   (list :height zenburned-height-plus-1))))))
   `(org-level-5 ((t (:inherit ,z-variable-pitch :foreground ,zenburned-cyan))))
   `(org-level-6 ((t (:inherit ,z-variable-pitch :foreground ,zenburned-green+2))))
   `(org-level-7 ((t (:inherit ,z-variable-pitch :foreground ,zenburned-red+2))))
   `(org-level-8 ((t (:inherit ,z-variable-pitch :foreground ,zenburned-magenta))))
   `(org-link ((t (:foreground ,zenburned-yellow-2 :underline t))))
   `(org-quote ((t (:background ,zenburned-bg+05 :extend t))))
   `(org-scheduled ((t (:foreground ,zenburned-green+4))))
   `(org-scheduled-previously ((t (:foreground ,zenburned-red))))
   `(org-scheduled-today ((t (:foreground ,zenburned-blue+1))))
   `(org-sexp-date ((t (:foreground ,zenburned-blue+1 :underline t))))
   `(org-special-keyword ((t (:inherit font-lock-comment-face))))
   `(org-table ((t (:foreground ,zenburned-green+2))))
   `(org-tag ((t (:weight bold :weight bold))))
   `(org-time-grid ((t (:foreground ,zenburned-orange))))
   `(org-todo ((t (:weight bold :foreground ,zenburned-red :weight bold))))
   `(org-upcoming-deadline ((t (:inherit font-lock-keyword-face))))
   `(org-warning ((t (:weight bold :foreground ,zenburned-red :weight bold :underline nil))))
   `(org-column ((t (:background ,zenburned-bg-1))))
   `(org-column-title ((t (:background ,zenburned-bg-1 :underline t :weight bold))))
   `(org-mode-line-clock ((t (:foreground ,zenburned-fg :background ,zenburned-bg-1))))
   `(org-mode-line-clock-overrun ((t (:foreground ,zenburned-bg :background ,zenburned-red-1))))
   `(org-ellipsis ((t (:foreground ,zenburned-yellow-1 :underline t))))
   `(org-footnote ((t (:foreground ,zenburned-cyan :underline t))))
   `(org-document-title ((t (:inherit ,z-variable-pitch :foreground ,zenburned-blue
                                      :weight bold
                                      ,@(when zenburned-scale-org-headlines
                                          (list :height zenburned-height-plus-4))))))
   `(org-document-info ((t (:foreground ,zenburned-blue))))
   `(org-habit-ready-face ((t :background ,zenburned-green)))
   `(org-habit-alert-face ((t :background ,zenburned-yellow-1 :foreground ,zenburned-bg)))
   `(org-habit-clear-face ((t :background ,zenburned-blue-3)))
   `(org-habit-overdue-face ((t :background ,zenburned-red-3)))
   `(org-habit-clear-future-face ((t :background ,zenburned-blue-4)))
   `(org-habit-ready-future-face ((t :background ,zenburned-green-2)))
   `(org-habit-alert-future-face ((t :background ,zenburned-yellow-2 :foreground ,zenburned-bg)))
   `(org-habit-overdue-future-face ((t :background ,zenburned-red-4)))
;;;;; org-ref
   `(org-ref-ref-face ((t :underline t)))
   `(org-ref-label-face ((t :underline t)))
   `(org-ref-cite-face ((t :underline t)))
   `(org-ref-glossary-face ((t :underline t)))
   `(org-ref-acronym-face ((t :underline t)))
;;;;; outline
   `(outline-1 ((t (:inherit ,z-variable-pitch :foreground ,zenburned-orange
                             ,@(when zenburned-scale-outline-headlines
                                 (list :height zenburned-height-plus-4))))))
   `(outline-2 ((t (:inherit ,z-variable-pitch :foreground ,zenburned-green+4
                             ,@(when zenburned-scale-outline-headlines
                                 (list :height zenburned-height-plus-3))))))
   `(outline-3 ((t (:inherit ,z-variable-pitch :foreground ,zenburned-blue-1
                             ,@(when zenburned-scale-outline-headlines
                                 (list :height zenburned-height-plus-2))))))
   `(outline-4 ((t (:inherit ,z-variable-pitch :foreground ,zenburned-yellow-2
                             ,@(when zenburned-scale-outline-headlines
                                 (list :height zenburned-height-plus-1))))))
   `(outline-5 ((t (:inherit ,z-variable-pitch :foreground ,zenburned-cyan))))
   `(outline-6 ((t (:inherit ,z-variable-pitch :foreground ,zenburned-green+2))))
   `(outline-7 ((t (:inherit ,z-variable-pitch :foreground ,zenburned-red-4))))
   `(outline-8 ((t (:inherit ,z-variable-pitch :foreground ,zenburned-blue-4))))
;;;;; p4
   `(p4-depot-added-face ((t :inherit diff-added)))
   `(p4-depot-branch-op-face ((t :inherit diff-changed)))
   `(p4-depot-deleted-face ((t :inherit diff-removed)))
   `(p4-depot-unmapped-face ((t :inherit diff-changed)))
   `(p4-diff-change-face ((t :inherit diff-changed)))
   `(p4-diff-del-face ((t :inherit diff-removed)))
   `(p4-diff-file-face ((t :inherit diff-file-header)))
   `(p4-diff-head-face ((t :inherit diff-header)))
   `(p4-diff-ins-face ((t :inherit diff-added)))
;;;;; c/perl
   `(cperl-nonoverridable-face ((t (:foreground ,zenburned-magenta))))
   `(cperl-array-face ((t (:foreground ,zenburned-yellow, :background ,zenburned-bg))))
   `(cperl-hash-face ((t (:foreground ,zenburned-yellow-1, :background ,zenburned-bg))))
;;;;; paren-face
   `(parenthesis ((t (:foreground ,zenburned-fg-1))))
;;;;; perspective
   `(persp-selected-face ((t (:foreground ,zenburned-yellow-2))))
;;;;; powerline
   `(powerline-active1 ((t (:background ,zenburned-bg-05 :inherit mode-line))))
   `(powerline-active2 ((t (:background ,zenburned-bg+2 :inherit mode-line))))
   `(powerline-inactive1 ((t (:background ,zenburned-bg+1 :inherit mode-line-inactive))))
   `(powerline-inactive2 ((t (:background ,zenburned-bg+3 :inherit mode-line-inactive))))
;;;;; proofgeneral
   `(proof-active-area-face ((t (:underline t))))
   `(proof-boring-face ((t (:foreground ,zenburned-fg :background ,zenburned-bg+2))))
   `(proof-command-mouse-highlight-face ((t (:inherit proof-mouse-highlight-face))))
   `(proof-debug-message-face ((t (:inherit proof-boring-face))))
   `(proof-declaration-name-face ((t (:inherit font-lock-keyword-face :foreground nil))))
   `(proof-eager-annotation-face ((t (:foreground ,zenburned-bg :background ,zenburned-orange))))
   `(proof-error-face ((t (:foreground ,zenburned-fg :background ,zenburned-red-4))))
   `(proof-highlight-dependency-face ((t (:foreground ,zenburned-bg :background ,zenburned-yellow-1))))
   `(proof-highlight-dependent-face ((t (:foreground ,zenburned-bg :background ,zenburned-orange))))
   `(proof-locked-face ((t (:background ,zenburned-blue-5))))
   `(proof-mouse-highlight-face ((t (:foreground ,zenburned-bg :background ,zenburned-orange))))
   `(proof-queue-face ((t (:background ,zenburned-red-4))))
   `(proof-region-mouse-highlight-face ((t (:inherit proof-mouse-highlight-face))))
   `(proof-script-highlight-error-face ((t (:background ,zenburned-red-2))))
   `(proof-tacticals-name-face ((t (:inherit font-lock-constant-face :foreground nil :background ,zenburned-bg))))
   `(proof-tactics-name-face ((t (:inherit font-lock-constant-face :foreground nil :background ,zenburned-bg))))
   `(proof-warning-face ((t (:foreground ,zenburned-bg :background ,zenburned-yellow-1))))
;;;;; racket-mode
   `(racket-keyword-argument-face ((t (:inherit font-lock-constant-face))))
   `(racket-selfeval-face ((t (:inherit font-lock-type-face))))
;;;;; rainbow-delimiters
   `(rainbow-delimiters-depth-1-face ((t (:foreground ,zenburned-fg))))
   `(rainbow-delimiters-depth-2-face ((t (:foreground ,zenburned-green+4))))
   `(rainbow-delimiters-depth-3-face ((t (:foreground ,zenburned-yellow-2))))
   `(rainbow-delimiters-depth-4-face ((t (:foreground ,zenburned-cyan))))
   `(rainbow-delimiters-depth-5-face ((t (:foreground ,zenburned-green+2))))
   `(rainbow-delimiters-depth-6-face ((t (:foreground ,zenburned-blue+1))))
   `(rainbow-delimiters-depth-7-face ((t (:foreground ,zenburned-yellow-1))))
   `(rainbow-delimiters-depth-8-face ((t (:foreground ,zenburned-green+1))))
   `(rainbow-delimiters-depth-9-face ((t (:foreground ,zenburned-blue-2))))
   `(rainbow-delimiters-depth-10-face ((t (:foreground ,zenburned-orange))))
   `(rainbow-delimiters-depth-11-face ((t (:foreground ,zenburned-green))))
   `(rainbow-delimiters-depth-12-face ((t (:foreground ,zenburned-blue-5))))
;;;;; rcirc
   `(rcirc-my-nick ((t (:foreground ,zenburned-blue))))
   `(rcirc-other-nick ((t (:foreground ,zenburned-orange))))
   `(rcirc-bright-nick ((t (:foreground ,zenburned-blue+1))))
   `(rcirc-dim-nick ((t (:foreground ,zenburned-blue-2))))
   `(rcirc-server ((t (:foreground ,zenburned-green))))
   `(rcirc-server-prefix ((t (:foreground ,zenburned-green+1))))
   `(rcirc-timestamp ((t (:foreground ,zenburned-green+2))))
   `(rcirc-nick-in-message ((t (:foreground ,zenburned-yellow))))
   `(rcirc-nick-in-message-full-line ((t (:weight bold))))
   `(rcirc-prompt ((t (:foreground ,zenburned-yellow :weight bold))))
   `(rcirc-track-nick ((t (:inverse-video t))))
   `(rcirc-track-keyword ((t (:weight bold))))
   `(rcirc-url ((t (:weight bold))))
   `(rcirc-keyword ((t (:foreground ,zenburned-yellow :weight bold))))
;;;;; re-builder
   `(reb-match-0 ((t (:foreground ,zenburned-bg :background ,zenburned-magenta))))
   `(reb-match-1 ((t (:foreground ,zenburned-bg :background ,zenburned-blue))))
   `(reb-match-2 ((t (:foreground ,zenburned-bg :background ,zenburned-orange))))
   `(reb-match-3 ((t (:foreground ,zenburned-bg :background ,zenburned-red))))
;;;;; realgud
   `(realgud-overlay-arrow1 ((t (:foreground ,zenburned-green))))
   `(realgud-overlay-arrow2 ((t (:foreground ,zenburned-yellow))))
   `(realgud-overlay-arrow3 ((t (:foreground ,zenburned-orange))))
   `(realgud-bp-enabled-face ((t (:inherit error))))
   `(realgud-bp-disabled-face ((t (:inherit secondary-selection))))
   `(realgud-bp-line-enabled-face ((t (:box (:color ,zenburned-red :style nil)))))
   `(realgud-bp-line-disabled-face ((t (:box (:color "grey70" :style nil)))))
   `(realgud-line-number ((t (:foreground ,zenburned-yellow))))
   `(realgud-backtrace-number ((t (:foreground ,zenburned-yellow, :weight bold))))
;;;;; regex-tool
   `(regex-tool-matched-face ((t (:background ,zenburned-blue-4 :weight bold))))
;;;;; rmail
   `(rmail-highlight ((t (:foreground ,zenburned-yellow :weight bold))))
   `(rmail-header-name ((t (:foreground ,zenburned-blue))))
;;;;; rpm-mode
   `(rpm-spec-dir-face ((t (:foreground ,zenburned-green))))
   `(rpm-spec-doc-face ((t (:foreground ,zenburned-green))))
   `(rpm-spec-ghost-face ((t (:foreground ,zenburned-red))))
   `(rpm-spec-macro-face ((t (:foreground ,zenburned-yellow))))
   `(rpm-spec-obsolete-tag-face ((t (:foreground ,zenburned-red))))
   `(rpm-spec-package-face ((t (:foreground ,zenburned-red))))
   `(rpm-spec-section-face ((t (:foreground ,zenburned-yellow))))
   `(rpm-spec-tag-face ((t (:foreground ,zenburned-blue))))
   `(rpm-spec-var-face ((t (:foreground ,zenburned-red))))
;;;;; rst-mode
   `(rst-level-1-face ((t (:foreground ,zenburned-orange))))
   `(rst-level-2-face ((t (:foreground ,zenburned-green+1))))
   `(rst-level-3-face ((t (:foreground ,zenburned-blue-1))))
   `(rst-level-4-face ((t (:foreground ,zenburned-yellow-2))))
   `(rst-level-5-face ((t (:foreground ,zenburned-cyan))))
   `(rst-level-6-face ((t (:foreground ,zenburned-green-2))))
;;;;; selectrum
   `(selectrum-current-candidate ((t (:foreground ,zenburned-yellow :weight bold :underline t))))
   `(selectrum-primary-highlight ((t (:background ,zenburned-green-2))))
   `(selectrum-secondary-highlight ((t (:background ,zenburned-green))))
;;;;; sh-mode
   `(sh-heredoc     ((t (:foreground ,zenburned-yellow :weight bold))))
   `(sh-quoted-exec ((t (:foreground ,zenburned-red))))
;;;;; show-paren
   `(show-paren-mismatch ((t (:foreground ,zenburned-red+1 :background ,zenburned-bg+3 :weight bold))))
   `(show-paren-match ((t (:foreground ,zenburned-fg :background ,zenburned-bg+3 :weight bold))))
;;;;; smart-mode-line
   ;; use (setq sml/theme nil) to enable Zenburned for sml
   `(sml/global ((,class (:foreground ,zenburned-fg :weight bold))))
   `(sml/modes ((,class (:foreground ,zenburned-yellow :weight bold))))
   `(sml/minor-modes ((,class (:foreground ,zenburned-fg-1 :weight bold))))
   `(sml/filename ((,class (:foreground ,zenburned-yellow :weight bold))))
   `(sml/line-number ((,class (:foreground ,zenburned-blue :weight bold))))
   `(sml/col-number ((,class (:foreground ,zenburned-blue+1 :weight bold))))
   `(sml/position-percentage ((,class (:foreground ,zenburned-blue-1 :weight bold))))
   `(sml/prefix ((,class (:foreground ,zenburned-orange))))
   `(sml/git ((,class (:foreground ,zenburned-green+3))))
   `(sml/process ((,class (:weight bold))))
   `(sml/sudo ((,class  (:foreground ,zenburned-orange :weight bold))))
   `(sml/read-only ((,class (:foreground ,zenburned-red-2))))
   `(sml/outside-modified ((,class (:foreground ,zenburned-orange))))
   `(sml/modified ((,class (:foreground ,zenburned-red))))
   `(sml/vc-edited ((,class (:foreground ,zenburned-green+2))))
   `(sml/charging ((,class (:foreground ,zenburned-green+4))))
   `(sml/discharging ((,class (:foreground ,zenburned-red+1))))
;;;;; smartparens
   `(sp-show-pair-mismatch-face ((t (:foreground ,zenburned-red+1 :background ,zenburned-bg+3 :weight bold))))
   `(sp-show-pair-match-face ((t (:background ,zenburned-bg+3 :weight bold))))
;;;;; sml-mode-line
   '(sml-modeline-end-face ((t :inherit default :width condensed)))
;;;;; SLIME
   `(slime-repl-output-face ((t (:foreground ,zenburned-red))))
   `(slime-repl-inputed-output-face ((t (:foreground ,zenburned-green))))
   `(slime-error-face
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,zenburned-red)))
      (t
       (:underline ,zenburned-red))))
   `(slime-warning-face
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,zenburned-orange)))
      (t
       (:underline ,zenburned-orange))))
   `(slime-style-warning-face
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,zenburned-yellow)))
      (t
       (:underline ,zenburned-yellow))))
   `(slime-note-face
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,zenburned-green)))
      (t
       (:underline ,zenburned-green))))
   `(slime-highlight-face ((t (:inherit highlight))))
;;;;; SLY
   `(sly-mrepl-output-face ((t (:foreground ,zenburned-red))))
   `(sly-error-face
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,zenburned-red)))
      (t
       (:underline ,zenburned-red))))
   `(sly-warning-face
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,zenburned-orange)))
      (t
       (:underline ,zenburned-orange))))
   `(sly-style-warning-face
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,zenburned-yellow)))
      (t
       (:underline ,zenburned-yellow))))
   `(sly-note-face
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,zenburned-green)))
      (t
       (:underline ,zenburned-green))))
   `(sly-stickers-placed-face ((t (:foreground ,zenburned-fg :background ,zenburned-bg+3))))
;;;;; solaire
   `(solaire-default-face ((t (:inherit default :background ,zenburned-bg-08))))
   `(solaire-minibuffer-face ((t (:inherit default :background ,zenburned-bg-08))))
   `(solaire-hl-line-face ((t (:inherit hl-line :background ,zenburned-bg))))
   `(solaire-org-hide-face ((t (:inherit org-hide :background ,zenburned-bg-08))))
;;;;; speedbar
   `(speedbar-button-face ((t (:foreground ,zenburned-green+2))))
   `(speedbar-directory-face ((t (:foreground ,zenburned-cyan))))
   `(speedbar-file-face ((t (:foreground ,zenburned-fg))))
   `(speedbar-highlight-face ((t (:foreground ,zenburned-bg :background ,zenburned-green+2))))
   `(speedbar-selected-face ((t (:foreground ,zenburned-red))))
   `(speedbar-separator-face ((t (:foreground ,zenburned-bg :background ,zenburned-blue-1))))
   `(speedbar-tag-face ((t (:foreground ,zenburned-yellow))))
;;;;; swiper
   `(swiper-line-face ((t (:underline t))))
;;;;; sx
   `(sx-custom-button
     ((t (:background ,zenburned-fg :foreground ,zenburned-bg-1
                      :box (:line-width 3 :style released-button) :height 0.9))))
   `(sx-question-list-answers
     ((t (:foreground ,zenburned-green+3
                      :height 1.0 :inherit sx-question-list-parent))))
   `(sx-question-mode-accepted
     ((t (:foreground ,zenburned-green+3
                      :height 1.3 :inherit sx-question-mode-title))))
   '(sx-question-mode-content-face ((t (:inherit highlight))))
   `(sx-question-mode-kbd-tag
     ((t (:box (:color ,zenburned-bg-1 :line-width 3 :style released-button)
               :height 0.9 :weight semi-bold))))
;;;;; tabbar
   `(tabbar-button ((t (:foreground ,zenburned-fg
                                    :background ,zenburned-bg))))
   `(tabbar-selected ((t (:foreground ,zenburned-fg
                                      :background ,zenburned-bg
                                      :box (:line-width -1 :style pressed-button)))))
   `(tabbar-unselected ((t (:foreground ,zenburned-fg
                                        :background ,zenburned-bg+1
                                        :box (:line-width -1 :style released-button)))))
;;;;; tab-bar
   `(tab-bar ((t (:background ,zenburned-bg+1))))
   `(tab-bar-tab ((t (:foreground ,zenburned-fg
                                  :background ,zenburned-bg
                                  :weight bold
                                  :box (:line-width -1 :style released-button)))))
   `(tab-bar-tab-inactive ((t (:foreground ,zenburned-fg
                                           :background ,zenburned-bg+1
                                           :box (:line-width -1 :style released-button)))))
;;;;; tab-line
   `(tab-line ((t (:background ,zenburned-bg+1))))
   `(tab-line-tab ((t (:foreground ,zenburned-fg
                                  :background ,zenburned-bg
                                  :weight bold
                                  :box (:line-width -1 :style released-button)))))
   `(tab-line-tab-inactive ((t (:foreground ,zenburned-fg
                                           :background ,zenburned-bg+1
                                           :box (:line-width -1 :style released-button)))))
   `(tab-line-tab-current ((t (:foreground ,zenburned-fg
                                           :background ,zenburned-bg+1
                                           :box (:line-width -1 :style pressed-button)))))
;;;;; term
   `(term-color-black ((t (:foreground ,zenburned-bg
                                       :background ,zenburned-bg-1))))
   `(term-color-red ((t (:foreground ,zenburned-red-2
                                     :background ,zenburned-red-4))))
   `(term-color-green ((t (:foreground ,zenburned-green
                                       :background ,zenburned-green+2))))
   `(term-color-yellow ((t (:foreground ,zenburned-orange
                                        :background ,zenburned-yellow))))
   `(term-color-blue ((t (:foreground ,zenburned-blue-1
                                      :background ,zenburned-blue-4))))
   `(term-color-magenta ((t (:foreground ,zenburned-magenta
                                         :background ,zenburned-red))))
   `(term-color-cyan ((t (:foreground ,zenburned-cyan
                                      :background ,zenburned-blue))))
   `(term-color-white ((t (:foreground ,zenburned-fg
                                       :background ,zenburned-fg-1))))
   '(term-default-fg-color ((t (:inherit term-color-white))))
   '(term-default-bg-color ((t (:inherit term-color-black))))
;;;;; undo-tree
   `(undo-tree-visualizer-active-branch-face ((t (:foreground ,zenburned-fg+1 :weight bold))))
   `(undo-tree-visualizer-current-face ((t (:foreground ,zenburned-red-1 :weight bold))))
   `(undo-tree-visualizer-default-face ((t (:foreground ,zenburned-fg))))
   `(undo-tree-visualizer-register-face ((t (:foreground ,zenburned-yellow))))
   `(undo-tree-visualizer-unmodified-face ((t (:foreground ,zenburned-cyan))))
;;;;; vertico
   `(vertico-current ((t (:foreground ,zenburned-yellow :weight bold :underline t))))
;;;;; visual-regexp
   `(vr/group-0 ((t (:foreground ,zenburned-bg :background ,zenburned-green :weight bold))))
   `(vr/group-1 ((t (:foreground ,zenburned-bg :background ,zenburned-orange :weight bold))))
   `(vr/group-2 ((t (:foreground ,zenburned-bg :background ,zenburned-blue :weight bold))))
   `(vr/match-0 ((t (:inherit isearch))))
   `(vr/match-1 ((t (:foreground ,zenburned-yellow-2 :background ,zenburned-bg-1 :weight bold))))
   `(vr/match-separator-face ((t (:foreground ,zenburned-red :weight bold))))
;;;;; volatile-highlights
   `(vhl/default-face ((t (:background ,zenburned-bg-05))))
;;;;; web-mode
   `(web-mode-builtin-face ((t (:inherit ,font-lock-builtin-face))))
   `(web-mode-comment-face ((t (:inherit ,font-lock-comment-face))))
   `(web-mode-constant-face ((t (:inherit ,font-lock-constant-face))))
   `(web-mode-css-at-rule-face ((t (:foreground ,zenburned-orange ))))
   `(web-mode-css-prop-face ((t (:foreground ,zenburned-orange))))
   `(web-mode-css-pseudo-class-face ((t (:foreground ,zenburned-green+3 :weight bold))))
   `(web-mode-css-rule-face ((t (:foreground ,zenburned-blue))))
   `(web-mode-doctype-face ((t (:inherit ,font-lock-comment-face))))
   `(web-mode-folded-face ((t (:underline t))))
   `(web-mode-function-name-face ((t (:foreground ,zenburned-blue))))
   `(web-mode-html-attr-name-face ((t (:foreground ,zenburned-orange))))
   `(web-mode-html-attr-value-face ((t (:inherit ,font-lock-string-face))))
   `(web-mode-html-tag-face ((t (:foreground ,zenburned-cyan))))
   `(web-mode-keyword-face ((t (:inherit ,font-lock-keyword-face))))
   `(web-mode-preprocessor-face ((t (:inherit ,font-lock-preprocessor-face))))
   `(web-mode-string-face ((t (:inherit ,font-lock-string-face))))
   `(web-mode-type-face ((t (:inherit ,font-lock-type-face))))
   `(web-mode-variable-name-face ((t (:inherit ,font-lock-variable-name-face))))
   `(web-mode-server-background-face ((t (:background ,zenburned-bg))))
   `(web-mode-server-comment-face ((t (:inherit web-mode-comment-face))))
   `(web-mode-server-string-face ((t (:inherit web-mode-string-face))))
   `(web-mode-symbol-face ((t (:inherit font-lock-constant-face))))
   `(web-mode-warning-face ((t (:inherit font-lock-warning-face))))
   `(web-mode-whitespaces-face ((t (:background ,zenburned-red))))
;;;;; whitespace-mode
   `(whitespace-space ((t (:background ,zenburned-bg+1 :foreground ,zenburned-bg+1))))
   `(whitespace-hspace ((t (:background ,zenburned-bg+1 :foreground ,zenburned-bg+1))))
   `(whitespace-tab ((t (:background ,zenburned-red-1))))
   `(whitespace-newline ((t (:foreground ,zenburned-bg+1))))
   `(whitespace-trailing ((t (:background ,zenburned-red))))
   `(whitespace-line ((t (:background ,zenburned-bg :foreground ,zenburned-magenta))))
   `(whitespace-space-before-tab ((t (:background ,zenburned-orange :foreground ,zenburned-orange))))
   `(whitespace-indentation ((t (:background ,zenburned-yellow :foreground ,zenburned-red))))
   `(whitespace-empty ((t (:background ,zenburned-yellow))))
   `(whitespace-space-after-tab ((t (:background ,zenburned-yellow :foreground ,zenburned-red))))
;;;;; wanderlust
   `(wl-highlight-folder-few-face ((t (:foreground ,zenburned-red-2))))
   `(wl-highlight-folder-many-face ((t (:foreground ,zenburned-red-1))))
   `(wl-highlight-folder-path-face ((t (:foreground ,zenburned-orange))))
   `(wl-highlight-folder-unread-face ((t (:foreground ,zenburned-blue))))
   `(wl-highlight-folder-zero-face ((t (:foreground ,zenburned-fg))))
   `(wl-highlight-folder-unknown-face ((t (:foreground ,zenburned-blue))))
   `(wl-highlight-message-citation-header ((t (:foreground ,zenburned-red-1))))
   `(wl-highlight-message-cited-text-1 ((t (:foreground ,zenburned-red))))
   `(wl-highlight-message-cited-text-2 ((t (:foreground ,zenburned-green+2))))
   `(wl-highlight-message-cited-text-3 ((t (:foreground ,zenburned-blue))))
   `(wl-highlight-message-cited-text-4 ((t (:foreground ,zenburned-blue+1))))
   `(wl-highlight-message-header-contents-face ((t (:foreground ,zenburned-green))))
   `(wl-highlight-message-headers-face ((t (:foreground ,zenburned-red+1))))
   `(wl-highlight-message-important-header-contents ((t (:foreground ,zenburned-green+2))))
   `(wl-highlight-message-header-contents ((t (:foreground ,zenburned-green+1))))
   `(wl-highlight-message-important-header-contents2 ((t (:foreground ,zenburned-green+2))))
   `(wl-highlight-message-signature ((t (:foreground ,zenburned-green))))
   `(wl-highlight-message-unimportant-header-contents ((t (:foreground ,zenburned-fg))))
   `(wl-highlight-summary-answered-face ((t (:foreground ,zenburned-blue))))
   `(wl-highlight-summary-disposed-face ((t (:foreground ,zenburned-fg
                                                         :slant italic))))
   `(wl-highlight-summary-new-face ((t (:foreground ,zenburned-blue))))
   `(wl-highlight-summary-normal-face ((t (:foreground ,zenburned-fg))))
   `(wl-highlight-summary-thread-top-face ((t (:foreground ,zenburned-yellow))))
   `(wl-highlight-thread-indent-face ((t (:foreground ,zenburned-magenta))))
   `(wl-highlight-summary-refiled-face ((t (:foreground ,zenburned-fg))))
   `(wl-highlight-summary-displaying-face ((t (:underline t :weight bold))))
;;;;; which-func-mode
   `(which-func ((t (:foreground ,zenburned-green+4))))
;;;;; xcscope
   `(cscope-file-face ((t (:foreground ,zenburned-yellow :weight bold))))
   `(cscope-function-face ((t (:foreground ,zenburned-cyan :weight bold))))
   `(cscope-line-number-face ((t (:foreground ,zenburned-red :weight bold))))
   `(cscope-mouse-face ((t (:foreground ,zenburned-bg :background ,zenburned-blue+1))))
   `(cscope-separator-face ((t (:foreground ,zenburned-red :weight bold
                                            :underline t :overline t))))
;;;;; yascroll
   `(yascroll:thumb-text-area ((t (:background ,zenburned-bg-1))))
   `(yascroll:thumb-fringe ((t (:background ,zenburned-bg-1 :foreground ,zenburned-bg-1))))
   ))

;;; Theme Variables
(zenburned-with-color-variables
  (custom-theme-set-variables
   'zenburned
;;;;; ansi-color
   `(ansi-color-names-vector [,zenburned-bg ,zenburned-red ,zenburned-green ,zenburned-yellow
                                          ,zenburned-blue ,zenburned-magenta ,zenburned-cyan ,zenburned-fg])
;;;;; company-quickhelp
   `(company-quickhelp-color-background ,zenburned-bg+1)
   `(company-quickhelp-color-foreground ,zenburned-fg)
;;;;; fill-column-indicator
   `(fci-rule-color ,zenburned-bg-05)
;;;;; nrepl-client
   `(nrepl-message-colors
     '(,zenburned-red ,zenburned-orange ,zenburned-yellow ,zenburned-green ,zenburned-green+4
       ,zenburned-cyan ,zenburned-blue+1 ,zenburned-magenta))
;;;;; pdf-tools
   `(pdf-view-midnight-colors '(,zenburned-fg . ,zenburned-bg-05))
;;;;; vc-annotate
   `(vc-annotate-color-map
     '(( 20. . ,zenburned-red-1)
       ( 40. . ,zenburned-red)
       ( 60. . ,zenburned-orange)
       ( 80. . ,zenburned-yellow-2)
       (100. . ,zenburned-yellow-1)
       (120. . ,zenburned-yellow)
       (140. . ,zenburned-green-2)
       (160. . ,zenburned-green)
       (180. . ,zenburned-green+1)
       (200. . ,zenburned-green+2)
       (220. . ,zenburned-green+3)
       (240. . ,zenburned-green+4)
       (260. . ,zenburned-cyan)
       (280. . ,zenburned-blue-2)
       (300. . ,zenburned-blue-1)
       (320. . ,zenburned-blue)
       (340. . ,zenburned-blue+1)
       (360. . ,zenburned-magenta)))
   `(vc-annotate-very-old-color ,zenburned-magenta)
   `(vc-annotate-background ,zenburned-bg-1)
   ))

;;; Rainbow Support

(declare-function rainbow-mode 'rainbow-mode)
(declare-function rainbow-colorize-by-assoc 'rainbow-mode)

(defcustom zenburned-add-font-lock-keywords nil
  "Whether to add font-lock keywords for zenburned color names.

In buffers visiting library `zenburned-theme.el' the zenburned
specific keywords are always added, provided that library has
been loaded (because that is where the code that does it is
defined).  If you visit this file and only enable the theme,
then you have to turn `rainbow-mode' off and on again for the
zenburned-specific font-lock keywords to be used.

In all other Emacs-Lisp buffers this variable controls whether
this should be done.  This requires library `rainbow-mode'."
  :type 'boolean
  :group 'zenburned-theme)

(defvar zenburned-colors-font-lock-keywords nil)

(defun zenburned--rainbow-turn-on ()
  "Maybe also add font-lock keywords for zenburned colors."
  (when (and (derived-mode-p 'emacs-lisp-mode)
             (or zenburned-add-font-lock-keywords
                 (and (buffer-file-name)
                      (equal (file-name-nondirectory (buffer-file-name))
                             "zenburned-theme.el"))))
    (unless zenburned-colors-font-lock-keywords
      (setq zenburned-colors-font-lock-keywords
            `((,(regexp-opt (mapcar 'car zenburned-default-colors-alist) 'words)
               (0 (rainbow-colorize-by-assoc zenburned-default-colors-alist))))))
    (font-lock-add-keywords nil zenburned-colors-font-lock-keywords 'end)))

(defun zenburned--rainbow-turn-off ()
  "Also remove font-lock keywords for zenburned colors."
  (font-lock-remove-keywords nil zenburned-colors-font-lock-keywords))

(when (fboundp 'advice-add)
  (advice-add 'rainbow-turn-on :after  #'zenburned--rainbow-turn-on)
  (advice-add 'rainbow-turn-off :after #'zenburned--rainbow-turn-off))

;;; Footer

;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))

(provide-theme 'zenburned)

;; Local Variables:
;; no-byte-compile: t
;; indent-tabs-mode: nil
;; eval: (when (require 'rainbow-mode nil t) (rainbow-mode 1))
;; End:
;;; zenburned-theme.el ends here
