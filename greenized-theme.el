;;; greenized.el --- The green version of solarized (fork of naysayer color theme)

;; Original Author Nick Aversano <nickav@users.noreply.github.com>
;; Author: Thiago Lopes <thiagolopes@pm.me>
;; Version: 0.1
;; Filename: greenized.el
;; Package-Requires: ((emacs "24"))
;; URL: https://github.com/thiagolopes/emacs.d
;; License: GPL-3+

;;; Commentary:
;; The green version of solarized.

;;; Code:
(unless (>= emacs-major-version 24)
  (error "The greenized theme requires Emacs 24 or later!"))

(deftheme greenized "The green solarized color theme.")

(defvar greenized-theme-default-text-color t
  "Enable secundary mood")
(defvar greenized-theme-mixed-fonts t
  "Disable italic")
(defvar greenized-theme-style-font nil
  "Style font")

;; TODO add options to: italic off, comentary style off, modeline border off;
(let ((background "#062626")
      (gutters    'background)
      (gutter-fg  'background)
      (gutters-active 'background)
      (builtin      "#c8d4ec")
      ;; (builtin      "#ffffff")
      ;; (selection  "#0000ff")
      (selection  "#071510")
      (text
       (if greenized-theme-default-text-color
           "#d3b58d"
         "#839496"))

      (comments "#3fdf1f")
      (strings    "#0fdfaf")
      (punctuation "lightgreen")
      (keywords "#ffffff")
      (functions "#ffffff")
      ;; (variables "#c1d1e3")
      ;; (methods    "#c1d1e3")
      (constants "#c8d4ec")
      (macros "#8cde94")
      (numbers "#0fddcc")
      (white "#ffffff")
      (error "#bb0000")
      (warning "#695a46")
      (highlight "darkseagreen2")
      (highlight-line "#0b3335")
      (line-fg "#126367")
      (black "#000000")
      (modeline "#d3b58d")
      (breadcrumb "#839496")
      (border-width '(2 . 2))
      (background-darker "#041e1e")
      (background-darker-darker "#041a1a")
      (md-fg "#888888")
      (md-bg "#111111")
      (md-text "#126367")
      (whitespace-fg "#0a3e3e")
      (smaller 0.8)

      (font-slant
       (if greenized-theme-mixed-fonts
           'italic
         'normal))

      ;; Solarized colors
      (yellow "#b58900")
      (orange "#cb4b16")
      (red "#dc322f")
      (magenta "#d33682")
      (blue "#268bd2")
      (green "#859900")
      (cyan "#2aa198")
      (violet "#6c71c4"))

  (defcustom greenized-theme-yellow  yellow  "Primary colors - yellow"  :type 'string :group 'solarized)
  (defcustom greenized-theme-orange  orange  "Primary colors - orange"  :type 'string :group 'solarized)
  (defcustom greenized-theme-red     red     "Primary colors - red"     :type 'string :group 'solarized)
  (defcustom greenized-theme-magenta magenta "Primary colors - magenta" :type 'string :group 'solarized)
  (defcustom greenized-theme-blue    blue    "Primary colors - blue"    :type 'string :group 'solarized)
  (defcustom greenized-theme-green   green   "Primary colors - green"   :type 'string :group 'solarized)
  (defcustom greenized-theme-cyan    cyan    "Primary colors - cyan"    :type 'string :group 'solarized)

  (custom-theme-set-faces
   'greenized



   ;; Default colors
   `(cursor                               ((t (:background ,punctuation))))
   `(default                              ((t (:foreground ,text :background ,background, :weight normal))))
   `(region                               ((t (:foreground nil :background ,selection :weight light))))
   `(fringe                               ((t (:background ,background :foreground ,white))))
   `(linum                                ((t (:background ,background :foreground ,gutter-fg))))
   `(highlight                            ((t (:foreground ,highlight :background ,selection))))



   ;; Font lock
   `(font-lock-constant-face              ((t (:foreground ,constants))))
   `(font-lock-variable-name-face         ((t (:foreground ,text))))
   (if greenized-theme-style-font
       `(font-lock-keyword-face ((t (:foreground ,keywords :slant ,font-slant))))
     `(font-lock-keyword-face ((t (:foreground ,keywords)))))
   (if greenized-theme-style-font
       `(font-lock-builtin-face ((t (:foreground ,builtin :slant ,font-slant))))
     `(font-lock-builtin-face ((t (:foreground ,builtin)))))

   `(font-lock-builtin-face               ((t (:foreground ,builtin  :slant ,font-slant))))
   `(font-lock-comment-face               ((t (:foreground ,comments))))
   `(font-lock-comment-delimiter-face     ((t (:foreground ,comments))))

   `(font-lock-type-face                  ((t (:foreground ,punctuation))))
   `(font-lock-string-face                ((t (:foreground ,strings))))
   `(font-lock-doc-face                   ((t (:foreground ,comments))))
   `(font-lock-function-name-face         ((t (:foreground ,functions))))
   `(font-lock-doc-string-face            ((t (:foreground ,strings))))
   `(font-lock-preprocessor-face          ((t (:foreground ,macros))))
   `(font-lock-warning-face               ((t (:foreground ,warning))))

   ;; Minibuffer
   `(minibuffer-prompt                    ((t (:foreground ,comments))))

   ;; Whitespace
   `(whitespace-space                     ((t (:foreground ,whitespace-fg))))

   ;; Error, Warning, Info, Success
   `(error                                ((t (:foreground ,red))))
   `(warning                              ((t (:foreground ,yellow))))
   `(info                                 ((t (:foreground ,magenta))))
   `(success                              ((t (:foreground ,green))))
   `(tooltip                              ((t (:foreground ,white :background ,line-fg))))


   ;; Plugins
   `(trailing-whitespace                  ((t (:foreground nil :background ,warning))))
   `(whitespace-trailing                  ((t (:background nil :foreground ,warning :inverse-video t))))

   `(linum                                ((t (:foreground ,line-fg :background ,background))))
   `(linum-relative-current-face          ((t (:foreground ,white :background ,background))))
   `(line-number                          ((t (:inherit default :foreground ,line-fg :background ,background))))
   `(line-number-current-line             ((t (:inherit line-number :foreground ,white :background ,line-fg))))

   ;; hl-line-mode
   `(hl-line                              ((t (:background ,highlight-line))))
   `(hl-line-face                         ((t (:background ,highlight-line))))

   ;;page-break-lines
   `(page-break-lines                     ((t (:background ,background))))

   ;; which-func
   `(which-func                           ((t (:inverse-video unspecified
                                                              :underline unspecified
                                                              :foreground ,background
                                                              :weight normal))))

   ;; mode-line and powerline
   `(mode-line-inactive                   ((t (:background ,background :foreground ,highlight-line :box t))))
   `(mode-line-buffer-id                  ((t (:foreground ,md-text :distant-foreground ,highlight-line :text ,text))))
   `(mode-line                            ((t (:inverse-video unspecified
                                                              :underline unspecified
                                                              :foreground ,md-fg
                                                              :background ,md-bg))))
   `(powerline-active1                    ((t (:background ,text :foreground ,background))))
   `(powerline-active2                    ((t (:background ,text :foreground ,background))))
   `(powerline-inactive1                  ((t (:background ,background :foreground ,text))))
   `(powerline-inactive2                  ((t (:background ,background :foreground ,text))))

   ;; doom modeline
   `(doom-modeline-project-dir            ((t (:foreground ,background :weight normal))))
   `(doom-modeline-project-parent-dir     ((t (:background ,modeline :foreground nil :weight normal))))
   `(doom-modeline-buffer-file            ((t (:foreground ,background-darker-darker :weight bold))))
   `(doom-modeline-buffer-modified        ((t (:foreground ,background-darker-darker :weight normal))))
   `(doom-modeline-warning                ((t (:foreground ,error :weight normal))))
   `(doom-modeline-bar                    ((t (:background ,text))))
   `(doom-modeline-bar-inactive           ((t (:background ,background))))

   ;; sml
   `(sml/global                         ((t (:foreground ,background))))
   `(sml/modes                            ((t (:foreground ,strings))))

   ;; ace-window
   `(aw-leading-char-face                 ((t (:foreground ,red))))

   ;; Company
   `(company-tooltip                      ((t (:foreground ,white :background ,background-darker))))
   ;; `(company-scrollbar-fg              ((t (:background ,strings))))
   ;; `(company-scrollbar-bg              ((t (:background ,text))))
   `(company-tooltip-annotation-selection ((t (:foreground ,white :background ,selection))))
   `(company-tooltip-selection            ((t (:foreground ,white :background ,line-fg))))
   `(company-tooltip-common               ((t (:foreground ,text))))

   ;; CTRLF
   `(ctrlf-highlight-active               ((t (:background ,line-fg :foreground ,white))))
   `(ctrlf-highlight-passive              ((t (:background ,selection :foreground ,strings))))

   ;; Swipper
   `(swiper-line-face                     ((t (:background ,background-darker))))
   `(swiper-match-face-2                  ((t (:background ,strings :foreground ,black))))

   ;; breadcrumb
   `(breadcrumb-face                          ((t (:foreground ,breadcrumb))))

   ;; Flycheck
   `(flycheck-posframe-background-face    ((t (:background ,background-darker))))
   `(flycheck-posframe-border-face        ((t (:foreground ,background-darker))))
   `(flycheck-inline-error                ((t (:background ,background-darker))))
   `(flycheck-inline-warning              ((t (:background ,background-darker))))
   `(flycheck-inline-info                 ((t (:background ,background-darker))))
   `(flycheck-error
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,red)
                   :foreground unspecified
                   :background unspecified
                   :inherit unspecified))
      (t (:foreground ,red :weight normal :underline t))))
   `(flycheck-warning
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,yellow)
                   :foreground unspecified
                   :background unspecified
                   :inherit unspecified))
      (t (:forground ,yellow :weight normal :underline t))))
   `(flycheck-info
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,green)
                   :foreground unspecified
                   :background unspecified
                   :inherit unspecified))
      (t (:forground ,green :weight normal :underline t))))

   ;; Flymake
   `(flymake-error
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,red)
                   :foreground unspecified
                   :background unspecified
                   :inherit unspecified))
      (t (:foreground ,red :weight normal :underline t))))
   `(flymake-warning
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,yellow)
                   :foreground unspecified
                   :background unspecified
                   :inherit unspecified))
      (t (:forground ,yellow :weight normal :underline t))))
   `(flymake-note
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,green)
                   :foreground unspecified
                   :background unspecified
                   :inherit unspecified))
      (t (:forground ,green :weight normal :underline t))))

   ;; bm
   `(bm-persistent-face                   ((t (:background ,background-darker))))


   `(show-paren-match                     ((t (:background ,background-darker-darker
                                                           :foreground ,white
                                                           :bold t
                                                           :inverse-video nil))))
   `(solaire-default-face                 ((t (:background ,background-darker-darker))))

   `(symbol-overlay-default-face          ((t (:bold t :background ,background-darker-darker))))

   `(whitespace-space                     ((t (:background ,background :foreground ,line-fg))))

   ;; Ido
   `(ido-first-match                      ((t (:foreground ,white :bold t))))

   ;; LSP
   `(lsp-face-highlight-textual           ((t (:background ,background-darker))))

   ;; Eglot
   `(eglot-mode-line                      ((t (:foreground ,background :weight normal))))

   ;; Pulsar
   `(pulsar-generic                       ((t (:background ,selection))))
   `(pulsar-red                           ((t (:background ,red))))
   `(pulsar-gree                          ((t (:background ,green))))
   `(pulsar-yellow                        ((t (:background ,yellow))))
   `(pulsar-blue                          ((t (:background ,blue))))
   `(pulsar-magenta                       ((t (:background ,magenta))))
   `(pulsar-cyan                          ((t (:background ,cyan))))

   ;; rainbow
   `(rainbow-delimiters-depth-1-face      ((t (:foreground ,violet))))
   `(rainbow-delimiters-depth-2-face      ((t (:foreground ,blue))))
   `(rainbow-delimiters-depth-3-face      ((t (:foreground ,green))))
   `(rainbow-delimiters-depth-4-face      ((t (:foreground ,yellow))))
   `(rainbow-delimiters-depth-5-face      ((t (:foreground ,orange))))
   `(rainbow-delimiters-depth-6-face      ((t (:foreground ,red))))
   `(rainbow-delimiters-depth-7-face      ((t (:foreground ,violet))))
   `(rainbow-delimiters-depth-8-face      ((t (:foreground ,blue))))
   `(rainbow-delimiters-depth-9-face      ((t (:foreground ,green))))
   `(rainbow-delimiters-depth-10-face     ((t (:foreground ,yellow))))
   `(rainbow-delimiters-depth-11-face     ((t (:foreground ,orange))))
   `(rainbow-delimiters-depth-12-face     ((t (:foreground ,red))))

   ;; eshell
   `(eshell-prompt                        ((t (:foreground ,constants :weight bold))))
   `(eshell-ls-archive                    ((t (:foreground ,red :weight bold))))
   `(eshell-ls-backup                     ((t (:inherit font-lock-comment-face))))
   `(eshell-ls-clutter                    ((t (:inherit font-lock-comment-face))))
   `(eshell-ls-directory                  ((t (:foreground ,blue :weight bold))))
   `(eshell-ls-executable                 ((t (:foreground ,green :weight bold))))
   `(eshell-ls-unreadable                 ((t (:background ,background-darker))))
   `(eshell-ls-missing                    ((t (:inherit font-lock-warning-face))))
   `(eshell-ls-product                    ((t (:inherit font-lock-doc-face))))
   `(eshell-ls-special                    ((t (:foreground ,yellow :weight bold))))
   `(eshell-ls-symlink                    ((t (:foreground ,cyan :weight bold))))

   ;; avy
   `(avy-lead-face                        ((t (:foreground ,white :background ,red :weight bold))))

   ;; js2-mode
   `(js2-function-call                    ((t (:inherit (font-lock-function-name-face)))))
   `(js2-function-param                   ((t (:foreground ,text))))
   `(js2-jsdoc-tag                        ((t (:foreground ,keywords))))
   `(js2-jsdoc-type                       ((t (:foreground ,constants))))
   `(js2-jsdoc-value                      ((t (:foreground ,text))))
   `(js2-object-property                  ((t (:foreground ,text))))
   `(js2-external-variable                ((t (:foreground ,constants))))
   `(js2-error                            ((t (:foreground ,error))))
   `(js2-warning                          ((t (:foreground ,warning))))

   ;; highlight numbers
   `(highlight-numbers-number             ((t (:foreground ,numbers))))

   ;; tab-bar-mode
   `(tab-bar                              ((t (:inherit modeline))))
   `(tab-bar-tab                          ((t (:foreground ,background :background ,text))))
   `(tab-bar-tab-inactive                 ((t (:foreground ,text :background ,background))))
   )

  (custom-theme-set-variables
   'greenized
   '(linum-format " %5i ")
   )
  )

;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))


(provide-theme 'greenized)

;; Local Variables:
;; no-byte-compile: t
;; End:

(provide 'greenized-theme)
;;; greenized-theme.el ends here
